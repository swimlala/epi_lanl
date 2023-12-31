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
resolution        :�o     �  qt   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �\   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �\   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �P   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �$   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �(   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �,Argo profile    3.1 1.2 19500101000000  20181121041156  20190604094026  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051                            2C  D   APEX                            5368                            041511                          846 @����nc�1   @��͔��,@4NV�u�d�j~��#1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�33@���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CA�fCD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn�Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtL�Dy��D��D�J=D�x�D��D��D�2=D���D��RD�)D�NfD���DǕqD�
D�AHD�~fD��=D��D�9HD�\D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@��\@�A�HA>�HA^�HA~�HA�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�B�RB�RB�RB�RB'�RB/�RB7�RB?�RBG�RBO�RBW�RB_�RBg�RBo�RBw�RB�RB��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�zCC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cn�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��
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
C��=C��
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
C��
C��
C��
C��
D {�D ��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D	{�D	��D
{�D
��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D {�D ��D!{�D!��D"{�D"��D#{�D#��D${�D$��D%{�D%��D&{�D&��D'{�D'��D({�D(��D){�D)��D*{�D*��D+{�D+��D,{�D,��D-{�D-��D.{�D.��D/{�D/��D0{�D0��D1{�D1��D2{�D2��D3{�D3��D4{�D4��D5{�D5��D6{�D6��D7{�D7��D8{�D8��D9{�D9��D:{�D:��D;{�D;��D<{�D<��D={�D=��D>{�D>��D?{�D?��D@{�D@��DA{�DA��DB{�DB��DC{�DC��DD{�DD��DE{�DE��DF{�DF��DG{�DG��DH{�DH��DI{�DI��DJ{�DJ��DK{�DK��DL{�DL��DM{�DM��DN{�DN��DO{�DO��DP{�DP��DQ{�DQ��DR{�DR��DS{�DS��DT{�DT��DU{�DU��DV{�DV��DW{�DW��DX{�DX��DY{�DY��DZ{�DZ��D[{�D[��D\{�D\��D]{�D]��D^{�D^��D_{�D_��D`{�D`��Da{�Da��Db{�Db��Dc{�Dc��Dd{�Dd��De{�De��Df{�Df��Dg{�Dg��Dh{�Dh��Di{�Di��Dj{�Dj��Dk{�Dk��Dl{�Dl��Dm{�Dm��Dn{�Dn��Do{�Do��Dp{�Dp��Dq{�Dq��Dr{�Dr��Ds{�Ds��DtHRDy�HD��D�H D�v�D���D�RD�0 D���D��D�	�D�L)D��]DǓ4D��D�?D�|)D�� D���D�7D�}D�ɚ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�-A�1'A�33A�5?A�9XA�5?A�33A�33A�/A�(�A�&�A�$�A�$�A�&�A�&�A�&�A�+A�1A���A��yA�`BA� �A��yA�AÓuAÁAÁA�~�A�ffA�C�A�(�A���A�{A��uA�?}A��A���A�
=A�bA���A��uA�^5A�33A�bA��A���A�I�A��A���A�A���A�XA���A��9A��DA�bNA�E�A�=qA�5?A���A��-A���A�K�A�A�^5A���A��^A��RA�l�A���A��;A�G�A���A�S�A��A�"�A��^A�A�v�A�ffA��hA�p�A�Q�A���A�VA�x�A�v�A�~�A��PA�(�A�|�A��A�VA�ffA��TA�A�S�A���A�VA��7A�`BA�{A�ffA�"�A�1A�-A��wA��HA�n�A�C�A�p�A�bA~��A}�TA|��A{��AzE�Ay?}Aw`BAvbNAvJAu�FAuK�At�jAs�wAsoAr��Ar{Ao��Al�9AjbAh-Ae�;Ad1Ac%Ab��Aat�A`ȴA`v�A`1'A]�wAZQ�AY&�AXAVjAT�AQx�APQ�AO��AL9XAIO�AHVAG��AGXAG&�AE�ADM�ACƨAB��AAp�A>�+A=��A=
=A<�uA<{A:-A8�/A8bNA7�
A5VA2{A0E�A.�\A-�wA,  A*�RA*9XA)��A);dA(��A'�#A'VA&bA$~�A$-A#�mA#��A#��A#p�A#G�A#+A#
=A"�9A"E�A!hsA AhsA��A^5A��AbA�FAQ�A�RA��A�AQ�AbNA�A�FA�A��A  A�
A�A
ȴA
n�A
E�A	�hA��A=qA;dA��A1'Ap�A��A�7A �@��T@�&�@���@�Ĝ@��!@�?}@�  @�P@��y@���@�O�@�9@�Z@�9X@�o@�5?@�Ĝ@��@�33@��H@ꟾ@���@�x�@�j@�j@��m@畁@�l�@�"�@�~�@�@���@���@�ff@�G�@�O�@�7@���@��D@�V@�ƨ@�~�@؛�@���@ԓu@��@҇+@�@�%@Ѓ@�I�@�|�@���@�5?@̴9@�ff@ɩ�@�r�@��@���@Ǖ�@�"�@�ȴ@�=q@�z�@�\)@��@�7L@���@��`@���@���@�Z@�  @�ƨ@��@���@�J@��@�z�@�dZ@��@��+@�{@��-@��@���@�r�@���@��R@�O�@�j@�9X@�ƨ@�K�@�V@���@�hs@�V@��@�j@�r�@�  @�
=@�^5@�5?@�@���@��@��
@��P@���@�E�@�9X@�ȴ@�M�@���@�X@�X@�O�@�7L@��@��/@��9@��9@��j@��9@��D@�Q�@�1@�l�@�C�@��@���@��@���@�b@���@��@��;@���@��@���@�|�@�dZ@��@�^5@�-@��T@��-@���@���@��7@��`@�z�@���@�ƨ@��@�S�@��@��H@��@�@�@���@�C�@���@�C�@��@�@��H@��R@�ff@�=q@�J@���@��@���@��h@�p�@�`B@�G�@�hs@��^@�n�@�v�@�^5@�E�@�J@��#@�x�@�G�@���@���@���@���@�V@��D@�1'@�1@���@�ƨ@�l�@���@�n�@��@���@��7@�p�@�&�@���@��9@�j@�I�@�A�@�b@�  @��@�ƨ@��w@�"�@���@���@���@�O�@�V@��`@��9@�r�@�Q�@�1'@��P@��R@��\@�v�@�v�@�$�@�@��^@��^@���@���@��@�x�@��@��@�x�@�?}@��/@��@�  @�;d@��!@��+@�n�@��@�@��^@��@���@�b�@��@z��@q�@f3�@[�{@U`B@L�K@F�@A4@;�m@7�@0�[@&�H@|�@�a@�[@<6@�>@$t@	T�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A�-A�1'A�33A�5?A�9XA�5?A�33A�33A�/A�(�A�&�A�$�A�$�A�&�A�&�A�&�A�+A�1A���A��yA�`BA� �A��yA�AÓuAÁAÁA�~�A�ffA�C�A�(�A���A�{A��uA�?}A��A���A�
=A�bA���A��uA�^5A�33A�bA��A���A�I�A��A���A�A���A�XA���A��9A��DA�bNA�E�A�=qA�5?A���A��-A���A�K�A�A�^5A���A��^A��RA�l�A���A��;A�G�A���A�S�A��A�"�A��^A�A�v�A�ffA��hA�p�A�Q�A���A�VA�x�A�v�A�~�A��PA�(�A�|�A��A�VA�ffA��TA�A�S�A���A�VA��7A�`BA�{A�ffA�"�A�1A�-A��wA��HA�n�A�C�A�p�A�bA~��A}�TA|��A{��AzE�Ay?}Aw`BAvbNAvJAu�FAuK�At�jAs�wAsoAr��Ar{Ao��Al�9AjbAh-Ae�;Ad1Ac%Ab��Aat�A`ȴA`v�A`1'A]�wAZQ�AY&�AXAVjAT�AQx�APQ�AO��AL9XAIO�AHVAG��AGXAG&�AE�ADM�ACƨAB��AAp�A>�+A=��A=
=A<�uA<{A:-A8�/A8bNA7�
A5VA2{A0E�A.�\A-�wA,  A*�RA*9XA)��A);dA(��A'�#A'VA&bA$~�A$-A#�mA#��A#��A#p�A#G�A#+A#
=A"�9A"E�A!hsA AhsA��A^5A��AbA�FAQ�A�RA��A�AQ�AbNA�A�FA�A��A  A�
A�A
ȴA
n�A
E�A	�hA��A=qA;dA��A1'Ap�A��A�7A �@��T@�&�@���@�Ĝ@��!@�?}@�  @�P@��y@���@�O�@�9@�Z@�9X@�o@�5?@�Ĝ@��@�33@��H@ꟾ@���@�x�@�j@�j@��m@畁@�l�@�"�@�~�@�@���@���@�ff@�G�@�O�@�7@���@��D@�V@�ƨ@�~�@؛�@���@ԓu@��@҇+@�@�%@Ѓ@�I�@�|�@���@�5?@̴9@�ff@ɩ�@�r�@��@���@Ǖ�@�"�@�ȴ@�=q@�z�@�\)@��@�7L@���@��`@���@���@�Z@�  @�ƨ@��@���@�J@��@�z�@�dZ@��@��+@�{@��-@��@���@�r�@���@��R@�O�@�j@�9X@�ƨ@�K�@�V@���@�hs@�V@��@�j@�r�@�  @�
=@�^5@�5?@�@���@��@��
@��P@���@�E�@�9X@�ȴ@�M�@���@�X@�X@�O�@�7L@��@��/@��9@��9@��j@��9@��D@�Q�@�1@�l�@�C�@��@���@��@���@�b@���@��@��;@���@��@���@�|�@�dZ@��@�^5@�-@��T@��-@���@���@��7@��`@�z�@���@�ƨ@��@�S�@��@��H@��@�@�@���@�C�@���@�C�@��@�@��H@��R@�ff@�=q@�J@���@��@���@��h@�p�@�`B@�G�@�hs@��^@�n�@�v�@�^5@�E�@�J@��#@�x�@�G�@���@���@���@���@�V@��D@�1'@�1@���@�ƨ@�l�@���@�n�@��@���@��7@�p�@�&�@���@��9@�j@�I�@�A�@�b@�  @��@�ƨ@��w@�"�@���@���@���@�O�@�V@��`@��9@�r�@�Q�@�1'@��P@��R@��\@�v�@�v�@�$�@�@��^@��^@���@���@��@�x�@��@��@�x�@�?}@��/@��@�  @�;d@��!@��+@�n�@��@�@��^@��G�O�@�b�@��@z��@q�@f3�@[�{@U`B@L�K@F�@A4@;�m@7�@0�[@&�H@|�@�a@�[@<6@�>@$t@	T�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB:^B:^B:^B:^B:^B:^B9XB:^B:^B:^B:^B9XB9XB9XB9XB9XB9XB8RB7LB9XBT�BdZBjBn�Bq�Br�Br�Br�Bt�Bv�By�B� B�\B��B��B��B��B��B��B��B�B�3B�jB��BƨB��B�B��B  B
=BuB�B�B �B"�B&�B+B,B-B0!B1'B2-B33B1'B0!B'�B�B+B��B�B�NB�B�B��B�-B��B�hB�7B�Bx�BiyBP�B7LB)�B!�B�B
=B��B�B�B�NB�#B��B�dB��B�+Bk�BaHB\)BQ�BA�B0!B%�B{BB
�B
�5B
��B
��B
ȴB
�jB
��B
��B
�bB
�DB
�B
z�B
s�B
hsB
aHB
_;B
\)B
YB
T�B
N�B
J�B
H�B
A�B
2-B
�B
\B
B	��B	�B	�mB	�TB	�#B	�B	��B	��B	��B	��B	��B	�hB	�%B	w�B	ffB	^5B	W
B	E�B	6FB	2-B	/B	-B	)�B	!�B	�B	�B	�B	JB	B��B��B��B�B�sB�ZB�NB�;B��B��BŢB��B�wB�dB�XB�RB�LB�?B�9B�-B�'B�B�'B�'B�'B�'B�'B�-B�-B�-B�-B�-B�'B�'B�B�B��B��B��B��B��B��B�hB�PB�1B�B�B�B�B~�B|�Bx�Bs�Br�Bq�Bp�Bp�Bn�Bn�Bm�Bl�Bk�BiyBgmBffBe`Be`BffBe`Be`BffBhsBk�Bm�Bn�Bo�Bs�Bt�Bu�Bu�Bu�Bu�Bv�By�Bz�Bz�B{�B{�B|�B�B�B�B�B�B�+B�=B�DB�PB�\B�hB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�?B�FB�^B�dB�jB�qB�wB�wB�}BƨB��B��B��B�B�
B�B�B�B�)B�/B�5B�HB�`B�yB�B�B�B�B��B��B��B��B��B	B	1B	oB	�B	�B	�B	 �B	'�B	,B	,B	,B	-B	.B	/B	5?B	:^B	<jB	=qB	=qB	>wB	B�B	D�B	E�B	H�B	J�B	R�B	W
B	YB	[#B	]/B	^5B	]/B	]/B	^5B	_;B	_;B	_;B	_;B	`BB	bNB	ffB	jB	n�B	o�B	p�B	q�B	t�B	{�B	}�B	}�B	}�B	}�B	}�B	}�B	}�B	}�B	}�B	�B	�%B	�1B	�7B	�=B	�DB	�JB	�JB	�VB	�bB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�3B	�9B	�FB	�RB	�RB	�XB	�^B	�dB	�jB	�jB	�wB	�}B	��B	B	ÖB	ŢB	ɺB	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�B	�
B	�)B	�#B	�#B	�#B	�5B	�BB	�NB	�TB	�ZB	�fB	�mB	�mB	�mB	�mB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
%B
%B
%B
1B
1B
1B

=B
VB
\B
�B
"�B
,WB
4B
?�B
D�B
J�B
N�B
R�B
XEB
\�B
aB
j0B
poB
s�B
xB
y�B
|�B
~�B
�-11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B3nB3nB3pB3nB3pB3pB2iB3rB3nB3pB3nB2lB2iB2iB2lB2iB2nB1^B0aB2nBNB]lBc�Bg�Bj�Bk�Bk�Bk�Bm�Bo�Br�ByB�mB��B��B��B��B��B��B�	B�B�CB�xB��B��B�B�B��B�
BJB}B�B�B�B�B�B$B%B&B)-B*3B+4B,6B*-B)'B �B�B 9B��B�B�bB�/B�B��B�AB��B�|B�JBzBq�Bb�BI�B0hB#B�B�BUB�B��B�B�lB�BB��B��B�B�PBd�BZrBUTBKB:�B)IBB�B
�CB
��B
�eB
�'B
�B
��B
��B
�+B
��B
��B
�wB
|IB
tB
l�B
a�B
Z�B
XvB
U_B
RMB
N6B
HB
C�B
A�B
:�B
+iB
�B
�B	�VB	�B	��B	�B	ܔB	�`B	�@B	�0B	�B	��B	�/B	��B	��B	iB	qB	_�B	W{B	PTB	>�B	/�B	+yB	(gB	&YB	#KB	B	�B	�B	�B	�B�YB�BB�(B�B��B��BݨBۜB،B�HB�B��B��B��B��B��B��B��B��B��B�B�yB�pB�|B�}B�xB�zB�|B��B��B��B��B��B�zB�{B�lB�_B�LB�@B�%B�B�B��B��B��B��B|iB|gB{cBz_BxQBvCBr,BmBlBkBi�Bi�Bg�Bg�Bf�Be�Bd�Bb�B`�B_�B^�B^�B_�B^�B^�B_�Ba�Bd�Bf�Bg�Bh�BmBnBoBoBoBo!Bp&Bs7Bt:Bt<BuDBu>BvGBz`B~yB~}B~wB~{B��B��B��B��B��B��B��B��B��B��B�B�(B�B�B�'B�;B�FB�KB�:B�=B�FB�VB�^B�cB�iB�hB�gB�tB��B��B��B��B��B��B��B��B��B�B�B�EB�SB�^B�fB�gB�oB�uBՂBֈB׏BڟB޺B��B��B��B� B�B�B�#B�>B�HB�NB�dB	�B	�B	�B	�B	B	B	!EB	%`B	%\B	%^B	&aB	'iB	(oB	.�B	3�B	5�B	6�B	6�B	7�B	;�B	=�B	>�B	B	B	DB	LFB	P]B	RkB	TuB	V�B	W�B	V~B	V�B	W�B	X�B	X�B	X�B	X�B	Y�B	[�B	_�B	c�B	g�B	h�B	i�B	j�B	nB	u:B	wEB	wFB	wFB	wFB	wEB	wEB	wCB	wFB	wIB	zZB	xB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�B	�"B	�:B	�FB	�CB	�\B	�wB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�5B	�>B	�?B	�IB	�@B	�EB	�OB	�KB	�EB	�KB	�RB	�VB	�xB	�oB	�qB	�qB	�B	ِB	ۚB	ܢB	ݨB	߳B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�
B	�B	�B	�B	�
B	�B	�(B	�%B	�(B	�%B	�.B	�:B	�<B	�<B	�<B	�:B	�?B	�JB	�LB	�NB	�NB	�NB	�ZB	�XB	�XB	�kB	�rB	�jB	�uB
zB
{B
~B
�G�O�B
�B
�B
B
%�B
-hB
9.B
=�B
DAB
G�B
L<B
Q�B
U�B
Z`B
cxB
i�B
mB
qhB
sB
u�B
w�B
|v11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.07 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0), vertically averaged dS =-0.007(+/-0.001) in PSS-78.                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Map scales: x=6,3; y=2,1. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                            201906040940262019060409402620190604094026  AO  ARCAADJP                                                                    20181121041156    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121041156  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121041156  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604094026  IP                  G�O�G�O�G�O�                