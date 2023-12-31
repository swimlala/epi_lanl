CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T04:11:56Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         (Matthew Alkire, University of Washington      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7,   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  74   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7t   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     88   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8X   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8x   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8|   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �|   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �|   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �0   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �4   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �D   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �H   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �L   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20181121041156  20190604094026  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051                            2C  D   APEX                            5368                            041511                          846 @��}Xr�1   @����X@48Q���d�p��
=1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B ffB  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDy�D�fD�G\D���D���D�
�D�H D��RD���D��D�U�D��fD��=D��D�5qDڟ�D��D��D�8�D��D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@�@�A�HA>�HA^�HA~�HA�p�A�p�A�p�A�p�A�p�A�p�A�p�B �B�RB�RB�RB�RB'�RB/�RB7�RB?�RBG�RBO�RBW�RB_�RBg�RBo�RBw�RB�RB��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B�\B�\B��)B��)B��)B��)B��)C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
D {�D ��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D	{�D	��D
{�D
��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D {�D ��D!{�D!��D"{�D"��D#{�D#��D${�D$��D%{�D%��D&{�D&��D'{�D'��D({�D(��D){�D)��D*{�D*��D+{�D+��D,{�D,��D-{�D-��D.{�D.��D/{�D/��D0{�D0��D1{�D1��D2{�D2��D3{�D3��D4{�D4��D5{�D5��D6{�D6��D7{�D7��D8{�D8��D9{�D9��D:{�D:��D;{�D;��D<{�D<��D={�D=��D>{�D>��D?{�D?��D@{�D@��DA{�DA��DB{�DB��DC{�DC��DD{�DD��DE{�DE��DF{�DF��DG{�DG��DH{�DH��DI{�DI��DJ{�DJ��DK{�DK��DL{�DL��DM{�DM��DN{�DN��DO{�DO��DP{�DP��DQ{�DQ��DR{�DR��DS{�DS��DT{�DT��DU{�DU��DV{�DV��DW{�DW��DX{�DX��DY{�DY��DZ{�DZ��D[{�D[��D\{�D\��D]{�D]��D^{�D^��D_{�D_��D`{�D`��Da{�Da��Db{�Db��Dc{�Dc��Dd{�Dd��De{�De��Df{�Df��Dg{�Dg��Dh{�Dh��Di{�Di��Dj{�Dj��Dk{�Dk��Dl{�Dl��Dm{�Dm��Dn{�Dn��Do{�Do��Dp{�Dp��Dq{�Dq��Dr{�Dr��Ds{�Ds��Dt{�Dt��Dy��D�)D�ED��]D���D��D�E�D��D�ҐD�qD�S�D��)D�� D���D�34DڝqD���D�HD�6gD�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�\)A�\)A�\)A�`BA�`BA�S�A�I�A�M�A�+A�bA�1A���A��;A�ƨAʸRAʴ9AʮAʡ�AʓuA�hsA�VA��A��HA��/A���Aɧ�Aɉ7AɃA�x�A�x�A�v�A�x�A�~�A�~�A�~�A�|�A�VA�E�A�=qA�;dA�+A��A�A��A��`A��/A��/A��#A��#A��#A��A���AȬAȋDAȇ+AȃA�jA�\)A�O�A��AǶFA�S�A��yA�Q�Aô9A��`A�^5A��!A��+A�oA��A�jA�I�A��yA��A��A��HA�v�A��`A�I�A���A�{A�;dA���A��A�oA�1A�
=A���A�n�A��A�~�A��+A�M�A�7LA��A���A���A��DA��^A�=qA��A���A��`A��A��#A���A��A�{A���A���A�{A��A�%A�l�A��A�K�A�bA��uA��HA�hsA���A�dZA~Q�A}�;Az-At=qAp��An�Am�Ak��Akp�Ak?}Aj��Aj�Ai��AiS�AgƨAdv�Aa�^A^��A[�7AYp�AX�9AX1AW��AW�AU
=AT9XAS��AR��AR�AOAO�ANr�AL��ALE�AL(�AL�AL1AKC�AG33AEG�AC�
AB��AA�^AAVA>�A=|�A=&�A<ZA;33A:�9A9��A9G�A9A8$�A6��A5��A5%A4bA3;dA2�A1�^A1�hA1K�A0��A0�A0=qA/33A-+A,-A+�A+l�A+?}A+%A*jA(A&�A%��A#p�A"ĜA"$�A!C�A ��A�AM�A��AQ�A �A�#AƨA�A��At�AhsA%A�+AA�A/A�!A��AoA��AI�A�^A�!A��Al�A��A{AC�A�AbNA
=A��AI�A�FA
�!A$�A-A��A\)A�A v�@�;d@��\@��T@�z�@�"�@�/@�9X@�@��
@�@�ff@�9@�bN@�-@�l�@�ȴ@�n�@��@�x�@���@�@�u@��@�@�z�@�n�@�&�@���@ى7@�b@��m@��;@ץ�@���@Լj@҇+@���@с@�O�@���@�^5@̋D@˕�@��H@ʸR@ʸR@ʰ!@ʟ�@ʏ\@�~�@�n�@�@��@ƸR@Ł@��@��@ģ�@�;d@�33@���@�x�@���@���@�j@� �@��@���@��@��@���@�(�@��@�C�@��@��+@��@��#@�x�@��@��@��@�-@���@�(�@���@�^5@�{@��@��-@�hs@�?}@�&�@�%@���@��/@��j@��D@���@��@���@��!@���@�v�@�{@�@��-@���@�p�@�G�@��/@��9@�bN@���@�{@�x�@��j@�1@�1@�1@��@�G�@��@�j@��m@�ȴ@�Ĝ@�t�@���@��H@�E�@��@��-@��7@��7@��7@��@�x�@�`B@�%@��`@���@���@��@��/@��@���@��@�/@��@��@��@��@��@��@�V@���@�Ĝ@��9@�9X@��
@���@���@���@��P@�C�@��@�o@���@���@�v�@�M�@�5?@�@��^@���@�@��h@�&�@��/@���@��j@���@�j@�ƨ@��@�;d@��@�$�@���@�x�@�X@�&�@��@��/@���@��@��@��9@���@��u@��@�1'@��;@�S�@�+@��@��@��\@�5?@�J@��@��^@��h@�hs@���@��@�9X@���@�ƨ@��F@�dZ@���@���@�~�@�ff@�=q@�J@��@���@��-@�x�@�`B@�?}@�/@�&�@�V@��/@��@�z�@�1'@�1@�  @��e@��Z@�ݘ@{�@p�[@i \@_�@Uϫ@P7�@J��@F�@?�@6��@0Ft@+�@#�@�@�3@ѷ@��@,=111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�\)A�\)A�\)A�`BA�`BA�S�A�I�A�M�A�+A�bA�1A���A��;A�ƨAʸRAʴ9AʮAʡ�AʓuA�hsA�VA��A��HA��/A���Aɧ�Aɉ7AɃA�x�A�x�A�v�A�x�A�~�A�~�A�~�A�|�A�VA�E�A�=qA�;dA�+A��A�A��A��`A��/A��/A��#A��#A��#A��A���AȬAȋDAȇ+AȃA�jA�\)A�O�A��AǶFA�S�A��yA�Q�Aô9A��`A�^5A��!A��+A�oA��A�jA�I�A��yA��A��A��HA�v�A��`A�I�A���A�{A�;dA���A��A�oA�1A�
=A���A�n�A��A�~�A��+A�M�A�7LA��A���A���A��DA��^A�=qA��A���A��`A��A��#A���A��A�{A���A���A�{A��A�%A�l�A��A�K�A�bA��uA��HA�hsA���A�dZA~Q�A}�;Az-At=qAp��An�Am�Ak��Akp�Ak?}Aj��Aj�Ai��AiS�AgƨAdv�Aa�^A^��A[�7AYp�AX�9AX1AW��AW�AU
=AT9XAS��AR��AR�AOAO�ANr�AL��ALE�AL(�AL�AL1AKC�AG33AEG�AC�
AB��AA�^AAVA>�A=|�A=&�A<ZA;33A:�9A9��A9G�A9A8$�A6��A5��A5%A4bA3;dA2�A1�^A1�hA1K�A0��A0�A0=qA/33A-+A,-A+�A+l�A+?}A+%A*jA(A&�A%��A#p�A"ĜA"$�A!C�A ��A�AM�A��AQ�A �A�#AƨA�A��At�AhsA%A�+AA�A/A�!A��AoA��AI�A�^A�!A��Al�A��A{AC�A�AbNA
=A��AI�A�FA
�!A$�A-A��A\)A�A v�@�;d@��\@��T@�z�@�"�@�/@�9X@�@��
@�@�ff@�9@�bN@�-@�l�@�ȴ@�n�@��@�x�@���@�@�u@��@�@�z�@�n�@�&�@���@ى7@�b@��m@��;@ץ�@���@Լj@҇+@���@с@�O�@���@�^5@̋D@˕�@��H@ʸR@ʸR@ʰ!@ʟ�@ʏ\@�~�@�n�@�@��@ƸR@Ł@��@��@ģ�@�;d@�33@���@�x�@���@���@�j@� �@��@���@��@��@���@�(�@��@�C�@��@��+@��@��#@�x�@��@��@��@�-@���@�(�@���@�^5@�{@��@��-@�hs@�?}@�&�@�%@���@��/@��j@��D@���@��@���@��!@���@�v�@�{@�@��-@���@�p�@�G�@��/@��9@�bN@���@�{@�x�@��j@�1@�1@�1@��@�G�@��@�j@��m@�ȴ@�Ĝ@�t�@���@��H@�E�@��@��-@��7@��7@��7@��@�x�@�`B@�%@��`@���@���@��@��/@��@���@��@�/@��@��@��@��@��@��@�V@���@�Ĝ@��9@�9X@��
@���@���@���@��P@�C�@��@�o@���@���@�v�@�M�@�5?@�@��^@���@�@��h@�&�@��/@���@��j@���@�j@�ƨ@��@�;d@��@�$�@���@�x�@�X@�&�@��@��/@���@��@��@��9@���@��u@��@�1'@��;@�S�@�+@��@��@��\@�5?@�J@��@��^@��h@�hs@���@��@�9X@���@�ƨ@��F@�dZ@���@���@�~�@�ff@�=q@�J@��@���@��-@�x�@�`B@�?}@�/@�&�@�V@��/@��@�z�@�1'@�1G�O�@��e@��Z@�ݘ@{�@p�[@i \@_�@Uϫ@P7�@J��@F�@?�@6��@0Ft@+�@#�@�@�3@ѷ@��@,=111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�!B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B�B��B��B��B��B��B��B��B��B��B�{B��B�-B�LB�LB�FB�LB�^BB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�
B�HB��B��BBJBbB{B"�B49B>wBF�BcTBw�B� B�B�DB��B��B��B�B�-B�qB�}B�dB�LB�9B�B��B��B��B��B��B�hB�+Bz�Bq�Bp�Bn�Bk�B^5BM�BI�BF�B?}B9XB)�B�BVB%B  B�B�B�qB�B��B~�Bp�B\)BO�BA�B�B1B
��B
�yB
��B
�}B
�B
��B
��B
�hB
�B
x�B
u�B
^5B
:^B
%�B
�B
oB
1B
%B
B
B	��B	��B	�B	�`B	��B	�}B	�B	��B	�hB	�PB	�7B	�+B	�B	v�B	q�B	m�B	iyB	cTB	W
B	R�B	N�B	E�B	C�B	B�B	A�B	@�B	9XB	'�B	�B	�B	oB	\B	
=B	B��B��B��B�B�B�B�yB�fB�NB�/B�B�B��B��B��B��B��BɺBǮBƨBĜB��B�qB�^B�XB�RB�LB�FB�3B�!B�B�B�B��B��B�B��B��B��B�B�B�B�?B�wB��BĜBƨBŢB��B��B�NB�sB�B��B	B	%B	1B	DB	JB	VB	DB	VB	PB	JB	1B	+B	+B	%B	B	B��B��B�B�B�`B�;B�#B�B�
B�B��B��B��B��B��BŢBĜBĜBÖB��B�qB�^B�LB�FB�?B�9B�3B�?B�RB�jB�}B�wB�jB�qB�wB��BŢBɺBɺBɺBɺB��B��B�B�B�#B�B�B�/B�BB�BB�HB�HB�HB�HB�HB�HB�HB�HB�HB�mB�B�B�B�B�B��B��B��B��B��B��B��B��B	  B	B	B	B	B	%B	%B	%B	%B	%B	+B	+B	+B		7B		7B	DB	oB	�B	'�B	-B	.B	.B	.B	.B	/B	0!B	1'B	2-B	33B	49B	5?B	6FB	<jB	A�B	B�B	B�B	A�B	A�B	B�B	C�B	C�B	C�B	C�B	D�B	E�B	E�B	F�B	I�B	M�B	O�B	R�B	VB	W
B	W
B	[#B	dZB	gmB	k�B	k�B	n�B	t�B	v�B	x�B	x�B	}�B	�B	�B	�7B	�7B	�7B	�=B	�DB	�bB	��B	��B	��B	��B	��B	�B	�-B	�9B	�FB	�RB	�^B	�^B	�dB	�dB	�jB	��B	B	ĜB	ǮB	ǮB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�B	�B	�B	�B	�B	�#B	�)B	�/B	�HB	�TB	�TB	�TB	�ZB	�ZB	�fB	�fB	�fB	�fB	�fB	�fB	�fB	�mB	�mB	�fB	�fB	�fB	�`B	�fB	�fB	�fB	�fB	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
uB
@B
�B
#�B
.}B
3�B
<�B
D�B
H�B
MB
QNB
U�B
\�B
a-B
e�B
m�B
r�B
t�B
y>B
}�B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B��B��B��B��B��B��B��B��B��B�B�zB�vB�nB�rB�wB�zB��B��B��B�sB�MB�?B�?B�GB�CB�)B�B�B�B��B��B��B��B��B��B�B�VB�bB�bB�fB�qB�dB�pB�qB�fB�aB�kB�{B�BͅBΎBЕB��B�LB��B��B�B	�BB\B-�B7�B@2B\�BqUBy�B~�B��B�B�^B�sB��B��B��B��B��B��B��B��B�oB�BB�+B�,B�B��B��BtkBk7Bj*Bh"BeBW�BG`BCJB@/B9B2�B#�B&B�B��B��B�0BϛB�B��B�!Bx�Bj8BU�BI{B;&B7B�B
��B
�B
ʇB
�B
��B
��B
�OB
�B
|�B
r�B
omB
W�B
4B
�B
MB
B
�B	��B	��B	��B	��B	�B	�`B	�B	ɏB	�/B	��B	�kB	�B	�B	��B	��B	{�B	p�B	kbB	gMB	c2B	]B	P�B	L�B	H�B	?ZB	=SB	<JB	;AB	:?B	3B	!�B	uB	LB	+B		B	�B��B��B��B��B�jB�XB�FB�7B�&B�B��B��B��B̴BɢBƎBŊBăB�|B�oB�lB�^B�GB�4B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�<B�LB�`B�kB�dBŎBͺB�B�6B�hB�B��B��B	�B	B	B	B	B	B	B	
B	�B	 �B	 �B��B��B��B��B�B�nB�EB�!B��B��B��B��B��B��B��B˶BȝBŎB�iB�eB�fB�]B�NB�6B�&B�B�B�B�B��B�B�B�0B�IB�@B�1B�:B�?B�JB�lBÂBÅBÃBÂBŏB˳B��B��B��B��B��B��B�B�	B�B�B�B�B�B�B�B�B�B�6B�HB�`B�cB�jB�lB�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B	 �B	 �B	 �B	 B	B	B	4B	B	!�B	&�B	'�B	'�B	'�B	'�B	(�B	)�B	*�B	+�B	,�B	-�B	/B	0	B	6/B	;NB	<TB	<RB	;NB	;OB	<SB	=ZB	=[B	=[B	=[B	>bB	?eB	?eB	@nB	C�B	G�B	I�B	L�B	O�B	P�B	P�B	T�B	^!B	a0B	eGB	eHB	h[B	nB	p�B	r�B	r�B	w�B	|�B	~�B	��B	��B	��B	� B	�B	� B	�MB	�nB	�zB	��B	��B	��B	��B	��B	�B	�B	�B	�B	�$B	�'B	�*B	�DB	�NB	�[B	�lB	�mB	ǏB	ʤB	̱B	̱B	̴B	̱B	λB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�#B	�#B	�)B	�$B	�&B	�'B	�%B	�+B	�,B	�%B	�#B	�&B	�B	�$B	�&B	�#B	�"B	�$B	�-B	�0B	�5B	�;B	�FB	�JB	�PB	�OB	�OB	�RB	�XB	�VB	�]B	�dB	�eB	�nB	�vB	�zB	�vB	�~B	��B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��G�O�B	�1B
 B
kB
�B
(7B
-�B
6xB
>VB
B�B
F�B
KB
OqB
V~B
Z�B
_�B
gKB
l�B
n�B
r�B
wzB
zt111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.07 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0), vertically averaged dS =-0.006(+/-0.001) in PSS-78.                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Map scales: x=6,3; y=2,1. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                            201906040940262019060409402620190604094026  AO  ARCAADJP                                                                    20181121041156    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121041156  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121041156  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604094026  IP                  G�O�G�O�G�O�                