CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-03-14T00:04:03Z creation      
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
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20180314000403  20190604094144  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051                            2C  D   APEX                            5368                            041511                          846 @�S4����1   @�S5�M�V@5S����d��t�j1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�  @�  A   A   A@  A`  A~ffA�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDyuD��D�F�D���D���D���D�O\D�v�D�ٚD�\D�FD�x D���D��D�ND�k�D��3D�D�R�D�s3D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @{�@�@�A�HA>�HA^�HA}G�A�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�B�RB�RB�RB�RB'�RB/�RB7�RB?�RBG�RBO�RBW�RB_�RBg�RBo�RBw�RB�RB��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B�\B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��
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
��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D {�D ��D!{�D!��D"{�D"��D#{�D#��D${�D$��D%{�D%��D&{�D&��D'{�D'��D({�D(��D){�D)��D*{�D*��D+{�D+��D,{�D,��D-{�D-��D.{�D.��D/{�D/��D0{�D0��D1{�D1��D2{�D2��D3{�D3��D4{�D4��D5{�D5��D6{�D6��D7{�D7��D8{�D8��D9{�D9��D:{�D:��D;{�D;��D<{�D<��D={�D=��D>{�D>��D?{�D?��D@{�D@��DA{�DA��DB{�DB��DC{�DC��DD{�DD��DE{�DE��DF{�DF��DG{�DG��DH{�DH��DI{�DI��DJ{�DJ��DK{�DK��DL{�DL��DM{�DM��DN{�DN��DO{�DO��DP{�DP��DQ{�DQ��DR{�DR��DS{�DS��DT{�DT��DU{�DU��DV{�DV��DW{�DW��DX{�DX��DY{�DY��DZ{�DZ��D[{�D[��D\{�D\��D]{�D]��D^{�D^��D_{�D_��D`{�D`��Da{�Da��Db{�Db��Dc{�Dc��Dd{�Dd��De{�De��Df{�Df��Dg{�Dg��Dh{�Dh��Di{�Di��Dj{�Dj��Dk{�Dk��Dl{�Dl��Dm{�Dm��Dn{�Dn��Do{�Do��Dp{�Dp��Dq{�Dq��Dr{�Dr��Ds{�Ds��Dt{�Dt��Dyp�D�qD�D{D���D��gD��]D�MD�t{D��]D�D�C�D�u�D�ؤD�
�D�K�D�iHD���D�
�D�P�D�p�D��{111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A��A��A��A��!A��!A��!A��-A��-A��-A��-A��9A��!A��7A�dZA��+A�33A�&�A�JA���A���A�^5A��A��jA��7A�K�A�
=A���A�hsA�S�A�5?A�$�A���A��wA��;A��+A���A�
=A�n�A�ƨA�A��A�=qA�A�$�A�/A�I�A���A��A��A�  A��A���A��A� �A�$�A���A�oA��A���A��A���A�Q�A��!A�=qA�x�A��A�dZA�O�A�33A���A��/A�n�A�33A�ȴA�K�A�;dA���A�ȴA�`BA�/A���A���A�{A�ĜA��uA���A���A�/A{&�Aw
=Au7LAt�9AsO�Ap��AoG�An��AnZAm��AlffAi+Af��AfbNAeG�AdȴAd��Ad1'Ac7LA`v�A\��A[%AYC�AW7LATbARM�AQl�ANv�AMt�AKAJ=qAJ{AI�;AIO�AG��AF  AC��AB �AAl�AA�A@ȴA?|�A>�A=`BA:��A8ȴA6�HA5�A4�!A4A�A3�hA1A/t�A.�A.n�A-��A-G�A,�A+��A)�^A&�9A#�wA"n�A!S�A �A��AI�A{A�AȴAQ�Ap�A33A%A�!A�A�PA�A��AQ�A�A?}A�uA�\A"�A�!AA�A�AK�A5?A�7A7LA�A�hA�AA�A��AA	��A9XAVA��AVA�A��AbNA��A"�@�"�@�p�@��@�ƨ@�7L@�&�@�P@���@�r�@��;@���@���@�\)@�u@柾@�&�@��@���@�-@�G�@�j@�l�@�n�@݁@�V@�z�@��@�~�@��@�x�@�z�@�v�@ՙ�@�x�@��@��
@�"�@�@Л�@�b@϶F@��@�-@��@���@̬@�1'@�+@�{@��`@�j@ǝ�@�;d@ƸR@���@�G�@�/@ċD@�l�@¸R@�@�7L@�%@��@�9X@�K�@��@�O�@�&�@�1@�;d@���@�=q@�G�@��@��P@��R@�V@�@��@���@��\@�=q@��@�p�@�7L@�%@���@�I�@���@���@�"�@�n�@��@�$�@��@��@�G�@��/@��D@��@��@�|�@��@�{@��@�O�@��@��@���@�j@�b@�ƨ@��@�;d@�o@�ȴ@�^5@��-@��@���@��@��D@��@���@��m@��w@���@��@�dZ@�+@��R@��!@�~�@�J@���@��T@���@���@��h@��7@��^@�V@��D@�j@�bN@��@��F@���@���@���@���@�\)@��@���@�=q@��#@���@��@�`B@�X@�X@�G�@�/@�V@��@�z�@�bN@��;@���@���@�t�@�\)@�S�@�
=@���@�M�@�@���@�O�@��@���@�Ĝ@��@�bN@�ƨ@�l�@�o@��@��y@��R@�ff@�=q@�{@���@��-@�x�@�`B@�X@�O�@�V@��@�r�@�bN@�1'@�ƨ@���@�33@��@��R@���@�E�@��@�J@���@���@��^@�p�@�7L@���@���@�Z@� �@��;@���@�C�@�;d@�+@�o@��@���@���@�V@�J@��@���@��-@��@�?}@��@�%@��`@���@�Ĝ@��D@�Q�@�(�@�1@��m@���@�dZ@�;d@�
=@��@�~�@�M�@��@��^@��h@�hs@�&�@��@��@�bN@�b@���@�;d@��@�
=@�@���@���@�ff@��@�	@|�@x�@pm�@h�@a��@W|�@M�@E|@>kQ@7{J@2#:@,H@'��@#Y@�@�@T�@Z@�@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A��A��A��A��!A��!A��!A��-A��-A��-A��-A��9A��!A��7A�dZA��+A�33A�&�A�JA���A���A�^5A��A��jA��7A�K�A�
=A���A�hsA�S�A�5?A�$�A���A��wA��;A��+A���A�
=A�n�A�ƨA�A��A�=qA�A�$�A�/A�I�A���A��A��A�  A��A���A��A� �A�$�A���A�oA��A���A��A���A�Q�A��!A�=qA�x�A��A�dZA�O�A�33A���A��/A�n�A�33A�ȴA�K�A�;dA���A�ȴA�`BA�/A���A���A�{A�ĜA��uA���A���A�/A{&�Aw
=Au7LAt�9AsO�Ap��AoG�An��AnZAm��AlffAi+Af��AfbNAeG�AdȴAd��Ad1'Ac7LA`v�A\��A[%AYC�AW7LATbARM�AQl�ANv�AMt�AKAJ=qAJ{AI�;AIO�AG��AF  AC��AB �AAl�AA�A@ȴA?|�A>�A=`BA:��A8ȴA6�HA5�A4�!A4A�A3�hA1A/t�A.�A.n�A-��A-G�A,�A+��A)�^A&�9A#�wA"n�A!S�A �A��AI�A{A�AȴAQ�Ap�A33A%A�!A�A�PA�A��AQ�A�A?}A�uA�\A"�A�!AA�A�AK�A5?A�7A7LA�A�hA�AA�A��AA	��A9XAVA��AVA�A��AbNA��A"�@�"�@�p�@��@�ƨ@�7L@�&�@�P@���@�r�@��;@���@���@�\)@�u@柾@�&�@��@���@�-@�G�@�j@�l�@�n�@݁@�V@�z�@��@�~�@��@�x�@�z�@�v�@ՙ�@�x�@��@��
@�"�@�@Л�@�b@϶F@��@�-@��@���@̬@�1'@�+@�{@��`@�j@ǝ�@�;d@ƸR@���@�G�@�/@ċD@�l�@¸R@�@�7L@�%@��@�9X@�K�@��@�O�@�&�@�1@�;d@���@�=q@�G�@��@��P@��R@�V@�@��@���@��\@�=q@��@�p�@�7L@�%@���@�I�@���@���@�"�@�n�@��@�$�@��@��@�G�@��/@��D@��@��@�|�@��@�{@��@�O�@��@��@���@�j@�b@�ƨ@��@�;d@�o@�ȴ@�^5@��-@��@���@��@��D@��@���@��m@��w@���@��@�dZ@�+@��R@��!@�~�@�J@���@��T@���@���@��h@��7@��^@�V@��D@�j@�bN@��@��F@���@���@���@���@�\)@��@���@�=q@��#@���@��@�`B@�X@�X@�G�@�/@�V@��@�z�@�bN@��;@���@���@�t�@�\)@�S�@�
=@���@�M�@�@���@�O�@��@���@�Ĝ@��@�bN@�ƨ@�l�@�o@��@��y@��R@�ff@�=q@�{@���@��-@�x�@�`B@�X@�O�@�V@��@�r�@�bN@�1'@�ƨ@���@�33@��@��R@���@�E�@��@�J@���@���@��^@�p�@�7L@���@���@�Z@� �@��;@���@�C�@�;d@�+@�o@��@���@���@�V@�J@��@���@��-@��@�?}@��@�%@��`@���@�Ĝ@��D@�Q�@�(�@�1@��m@���@�dZ@�;d@�
=@��@�~�@�M�@��@��^@��h@�hs@�&�@��@��@�bN@�b@���@�;d@��@�
=@�@���@���@�ffG�O�@�	@|�@x�@pm�@h�@a��@W|�@M�@E|@>kQ@7{J@2#:@,H@'��@#Y@�@�@T�@Z@�@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bx�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bx�Bw�Bz�B�1B�ZB�B7LBB�BH�BO�BT�B]/BffBl�Bp�Bq�Bw�B~�B�B�B�%B�+B�7B��B��B��B��B�PB�Bw�Bp�Bt�Bq�BgmBz�Br�Bu�Bv�Bn�Be`B_;BVBG�B<jB33B'�BJB�BɺB�jB��BȴB��BÖB�jB��B�\B~�BiyBL�B:^B(�B1BB
��B
�fB
�mB
�ZB
�B
��B
ǮB
��B
�9B
�B
��B
��B
�VB
s�B
W
B
.B
bB
B
  B	��B	�mB	�5B	�B	�B	��B	ŢB	�'B	��B	��B	��B	��B	�uB	�bB	�1B	v�B	bNB	XB	K�B	>wB	0!B	&�B	 �B	{B	VB	+B	B	B	  B��B��B�B�B�`B�TB�HB�5B�B��B��BB�dB�?B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B�{B�hB�VB�PB�JB�DB�7B�1B�+B�%B�B�B�B�B�B� B~�B}�B|�Bz�Bw�Bu�Bt�Bs�Br�Bq�Bp�Bo�Bn�Bm�Bl�Bk�BjBiyBhsBgmBffBe`BdZBdZBe`BgmBgmBffBe`BdZBdZBcTBbNBaHB^5B^5B]/B^5B_;B_;B^5B[#B\)B[#B[#B\)B^5B^5B^5B^5B^5B_;B`BBbNBbNBcTBcTBgmBhsBhsBiyBn�Bp�Bo�Bp�Bs�Bt�Bw�By�By�Bz�B{�B~�B�B�B�B�%B�7B�PB�hB�hB�{B�{B�{B��B��B��B��B��B��B��B�B�B�B�B�!B�9B�FB�LB�jB�wB��BBƨBȴB��B��B��B��B�
B�B�BB�TB�`B�sB�yB�B�B�B��B��B��B��B��B��B	  B	B	B		7B	
=B	PB	VB	\B	hB	uB	{B	{B	{B	{B	{B	�B	�B	�B	!�B	$�B	'�B	)�B	+B	1'B	49B	5?B	9XB	>wB	D�B	F�B	F�B	G�B	I�B	J�B	K�B	L�B	Q�B	R�B	T�B	[#B	\)B	]/B	^5B	`BB	bNB	dZB	gmB	jB	k�B	k�B	jB	k�B	n�B	p�B	r�B	t�B	t�B	w�B	{�B	}�B	�B	�+B	�1B	�7B	�=B	�=B	�=B	�DB	�JB	�PB	�\B	�bB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�3B	�9B	�9B	�FB	�XB	�^B	�^B	�dB	�dB	�jB	�wB	�wB	�wB	��B	B	ÖB	ĜB	ŢB	ǮB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�
B	�B	�B	�B	�/B	�5B	�;B	�BB	�HB	�TB	�TB	�TB	�ZB	�ZB	�`B	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B

=B

�B
mB
"�B
'mB
.B
72B
>�B
F?B
L�B
T�B
[�B
_!B
e�B
i*B
lqB
p;B
uB
y�B
~�B
��B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  Bh�Bh�Bh�Bh�Bh�Bh�Bh�Bh�Bh�Bh�Bh�Bh�Bh�Bh�Bh�Bh�Bh�Bh�Bi�Bh�Bh�Bh�Bh�Bh�Bh�Bh�Bh�Bh�Bi�Bh�Bk�ByB�<B�B(B3aB9�B@�BE�BM�BW6B]\BaoBbtBh�Bo�Bq�Bs�Bv�Bw�Bz B�cB��B��B�]B~Bs�Bh�BatBe�Bb}BX=Bk�Bc�Bf�Bg�B_mBV2BPBF�B8�B-EB$B�B�+B�B��B�VB�sB��B��B��B�WB��B�KBo�BZoB=�B+`B�B
�5B
�B
��B
�mB
�xB
�fB
�B
��B
��B
��B
�IB
�B
��B
��B
lB
d�B
H"B
5B
�B	�FB	�'B	��B	ؘB	�aB	�DB	�5B	�B	��B	�[B	�B	��B	��B	��B	��B	��B	yhB	hB	S�B	IMB	=
B	/�B	!eB	.B	B	�B��B�vB�XB�QB�MB�4B�B��B��BֱBԡBҘBφB�hB�MB�!B��B��B��B�qB�nB�_B�NB�9B�%B�&B�@B�LB�CB�>B�5B�B�B��B��B��B�B~�B}�B|�Bz�By�Bx�Bw�BvBvBtsBspBrgBqcBp_BoZBnPBlFBi6Bg*Bf!BeBdBcBb
BaB_�B^�B]�B\�B[�BZ�BY�BX�BW�BV�BU�BU�BV�BX�BX�BW�BV�BU�BU�BT�BS�BR�BO�BO�BN�BO�BP�BP�BO�BL�BM�BL�BL�BM�BO�BO�BO�BO�BO�BP�BQ�BS�BS�BT�BT�BX�BY�BY�BZ�B`BbBa	BbBe$Bf,Bi?BkIBkJBlNBmQBpiBt�Bu�Bv�Bw�Bz�B~�B��B��B��B��B��B��B��B�B�B�&B�6B�MB�nB�rB�xB�qB��B��B��B��B��B��B��B��B�B�B�.B�9B�LB�SB�oB˂BѩBԻB��B��B��B��B��B��B�B�!B�%B�8B�UB�]B�dB�|B��B��B��B��B��B	 �B	�B	�B	�B	�B	�B	�B	�B		�B	B	B	,B	=B	OB	ZB	cB	"�B	%�B	&�B	*�B	/�B	5�B	8 B	8B	9	B	;B	< B	=$B	>*B	CFB	DLB	FWB	L~B	M�B	N�B	O�B	Q�B	S�B	U�B	X�B	[�B	\�B	\�B	[�B	\�B	_�B	a�B	dB	fB	fB	i&B	m>B	oJB	sbB	x�B	y�B	z�B	{�B	{�B	{�B	|�B	}�B	~�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�%B	�3B	�;B	�BB	�HB	�HB	�OB	�dB	�rB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	� B	�0B	�5B	�CB	�KB	�RB	�UB	�VB	�ZB	�ZB	�bB	�oB	�nB	�|B	τB	ЉB	эB	ҔB	ԝB	ԟB	ԡB	ըB	դB	֭B	ײB	ؼB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�!B	�(B	�+B	�0B	�2B	�1B	�<B	�>B	�@B	�PB	�\B	�]B	�[B	�[B	�]B	�\B	�aG�O�B	�B
�B
�B
�B
\B
(yB
/�B
7�B
=�B
F+B
MB
PfB
WB
ZnB
]�B
a�B
fPB
kB
o�B
sB
u�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.07 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9996(+/-0.0001), vertically averaged dS =-0.014(+/-0.002) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Map scales: x=6,3; y=2,1. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                            201906040941442019060409414420190604094144  AO  ARCAADJP                                                                    20180314000403    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20180314000403  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20180314000403  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604094144  IP                  G�O�G�O�G�O�                