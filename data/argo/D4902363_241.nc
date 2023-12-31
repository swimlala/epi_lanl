CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-05-20T00:35:24Z creation;2018-05-20T00:35:30Z conversion to V3.1;2019-12-19T07:41:57Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7T   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     88   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8X   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8\   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8d   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8h   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8p   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8x   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        l  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \P   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  `,   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  st   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �p   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  �L   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  �$   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ې   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �    SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �    SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �    SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �    HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    � Argo profile    3.1 1.2 19500101000000  20180520003524  20200115131516  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0576_241                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�c��J� 1   @�c�I���@:.;�5�X�dP:)�y�1   GPS     A   B   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ Dϼ�D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D��3D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @5�@{�@�@�A�HA>�HA^�HA~�HA�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�B�RB�RB�RB�RB'�RB/�RB7�RB?�RBG�RBO�RBW�RB_�RBg�RBo�RBw�RB�RB��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
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
��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D {�D ��D!{�D!��D"{�D"��D#{�D#��D${�D$��D%{�D%��D&{�D&��D'{�D'��D({�D(��D){�D)��D*{�D*��D+{�D+��D,{�D,��D-{�D-��D.{�D.��D/{�D/��D0{�D0��D1{�D1��D2{�D2��D3{�D3��D4{�D4��D5{�D5��D6{�D6��D7{�D7��D8{�D8��D9{�D9��D:{�D:��D;{�D;��D<{�D<��D={�D=��D>{�D>��D?{�D?��D@{�D@��DA{�DA��DB{�DB��DC{�DC��DD{�DD��DE{�DE��DF{�DF��DG{�DG��DH{�DH��DI{�DI��DJ{�DJ��DK{�DK��DL{�DL��DM{�DM��DN{�DN��DO{�DO��DP{�DP��DQ{�DQ��DR{�DR��DS{�DS��DT{�DT��DU{�DU��DV{�DV��DW{�DW��DX{�DX��DY{�DY��DZ{�DZ��D[{�D[��D\{�D\��D]{�D]��D^{�D^��D_{�D_��D`{�D`��Da{�Da��Db{�Db��Dc{�Dc��Dd{�Dd��De{�De��Df{�Df��Dg{�Dg��Dh{�Dh��Di{�Di��Dj{�Dj��Dk{�Dk��Dl{�Dl��Dm{�Dm��Dn{�Dn��Do{�Do��Dp{�Dp��Dq{�Dq��Dr{�Dr��Ds{�Ds��Dt{�Dt��Du{�Du��Dv{�Dv��Dw{�Dw��Dx{�Dx��Dy{�Dy��Dz{�Dz��D{{�D{��D|{�D|��D}{�D}��D~{�D~��D{�D��D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D���D���D���D�=�D�}�D���D���D�=�D�}�D½�D���D�=�D�}�Dý�D���D�=�D�}�DĽ�D���D�=�D�}�DŽ�D���D�=�D�}�Dƽ�D���D�=�D�}�Dǽ�D���D�=�D�}�DȽ�D���D�=�D�}�Dɽ�D���D�=�D�}�Dʽ�D���D�=�D�}�D˽�D���D�=�D�}�D̽�D���D�=�D�}�Dͽ�D���D�=�D�}�Dν�D���D�=�D�}�DϺ�D���D�=�D�}�Dн�D���D�=�D�}�Dѽ�D���D�=�D�}�Dҽ�D���D�=�D�}�Dӽ�D���D�=�D�}�DԽ�D���D�=�D�}�Dս�D���D�=�D�}�Dֽ�D���D�=�D�}�D׽�D���D�=�D�}�Dؽ�D���D�=�D�}�Dٽ�D���D�=�D�}�Dڽ�D���D�=�D�}�D۽�D���D�=�D�}�Dܽ�D���D�=�D�}�Dݽ�D���D�=�D�}�D޽�D���D�=�D�}�D߽�D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D���D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D���D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��7A��A��A�~�A�ZA� �A�=qA��uA�jA�  A�~�A�M�A�JA��#A��wA���A���A��A�l�A�E�A�(�A���A�~�A��A�ƨA���A��A�jA�G�A���A���A�|�A��A���A�jA���A�n�A�M�A�K�A�ƨA��A�dZA��HA��uA���A�E�A���A�G�A�&�A�oA��RA��A�K�A��/A��A�C�A�l�A�I�A�?}A���A���A��DA��A���A�z�A�dZA��\A�=qA��A���A�hsA��A�&�A��#A�E�A�$�A�/A��+A�O�A���A�r�A�oA�33A��^A���A�"�A~�`A|5?Az�Ax1'Av��Au�-At-Ar�/ArbNAq��Ap�An�Al�Aj=qAi�AgAc�hAbv�Aat�A`�uA`1'A_�A_oA^ffA\�!AYl�AW/AV�/AVv�AU��AT��ATM�AS��AS��ASK�AQ�-AO�AO�-AO�^AO�^AO�-AO��AOXAOoAN�ANv�AL^5AK�#AJ��AJjAJ �AH�AH-AF��AF{AE��AD�uACp�ACoAB�RAA��AA�A@�A?�;A>�!A>(�A=��A<-A;C�A:�uA9�^A8�RA8  A7�^A7x�A7
=A6A3x�A21'A17LA0A�A0  A/�^A/A.  A-hsA-A,r�A+��A+�A*{A(��A't�A&��A%�TA$^5A"^5A!�mA!�^A!hsA!;dA!&�A!
=A n�A�-A;dA+A�9A��A~�A1A��A?}A5?A�A�A��A�wAffA��A-A�^AS�A��A�#A
=A�`A��A�+A�^A�PAv�At�A
ffA1'A�yAI�A�
A?}A��A�;A"�A �A`BA n�@�M�@��h@��@��9@�r�@�1'@��
@��P@��@���@�@�`B@�(�@���@��@�E�@���@�&�@�F@��@�9X@���@�r�@�@�!@�E�@��@��@�V@�b@�+@��y@���@���@�r�@߮@ާ�@��@�S�@���@�`B@�z�@�n�@ԃ@ӍP@�"�@���@ҧ�@Ѳ-@�(�@�dZ@�^5@��@�r�@ʗ�@ɲ-@�/@�l�@���@�%@���@+@��-@�G�@��`@�A�@�o@��@�&�@��`@�Ĝ@�1@��@�S�@�^5@���@��u@��P@�`B@���@�z�@� �@��@��@�dZ@�"�@��y@���@��@��@�(�@���@��F@�{@��@���@�C�@�
=@�-@��@�Q�@�ff@��T@�p�@�G�@���@�j@��P@���@�J@�/@��9@�Q�@�ƨ@���@���@��+@�^5@�J@�?}@�j@�ƨ@�"�@�v�@�J@��^@���@�I�@���@�\)@�+@��\@�{@��T@�G�@��j@��@�l�@��R@�-@��#@���@���@���@�bN@�I�@�1'@��@��@�"�@���@�E�@�@��T@���@���@�7L@��`@�z�@�1'@��@��@���@��P@�dZ@��@��!@�n�@�$�@��^@�O�@��`@���@���@���@��j@���@�bN@�Q�@�I�@�(�@�b@��@���@���@�S�@�33@��@�
=@���@���@�E�@���@�x�@�&�@��/@���@���@���@���@�I�@�w@~ȴ@}�@|��@|�/@|I�@{t�@{@z��@zn�@zM�@z=q@z=q@zJ@yhs@x�`@x�@xQ�@xb@w�w@w�@v�@v�R@v��@vV@v@u�T@u�-@u�h@u�@up�@u`B@uO�@u/@t�@tI�@s��@s�
@sƨ@s��@s33@r�!@r^5@r=q@q��@qx�@q�@p��@p��@o��@o�w@o�w@o�@o�@pQ�@q%@qG�@pr�@pQ�@o�@o�w@o\)@mp�@l�@l��@l�j@lI�@k��@j��@jn�@jM�@j�@i�@i��@ihs@i7L@h��@hr�@g�w@g\)@f�R@fff@fE�@e@e��@e��@e��@e��@e�h@e/@e�@e�-@e`B@e/@d��@d�j@c�
@cdZ@b�@b��@b�!@b�\@b^5@b-@bJ@a��@a�7@aX@a%@`�@`Q�@`A�@`  @_�w@_K�@^��@^��@^E�@]�T@]p�@]V@\�@\I�@[ƨ@Z�@Zn�@Y�#@Y��@Yx�@Y&�@X�9@XbN@XA�@Xb@W�w@W\)@V�y@V�+@V$�@U��@U�h@U/@T��@Sƨ@S33@R��@RM�@Q�^@QG�@PĜ@PQ�@O�P@N�y@Nv�@M�T@M�-@M?}@Lz�@Lj@LZ@LI�@L1@K�
@K�F@K��@K"�@J��@J=q@I��@I�#@I��@I�@H�9@H �@G��@G\)@GK�@GK�@G+@F�R@F��@F�+@F�+@F�+@F�+@Fff@Fff@F$�@E��@E�-@E��@E�h@E�@D�@D�j@DI�@D�@C�
@C��@CC�@C"�@B��@B��@B�!@B�\@BJ@A7L@@�9@@bN@@1'@@b@?�;@?|�@>��@>��@>v�@>ff@>5?@>$�@>$�@>{@=�@=��@=��@=�-@=`B@=V@<�@<��@<�j@<��@<�j@<�@<(�@;�m@;dZ@;"�@:�@:�H@:��@:�\@:=q@9��@9��@9��@9�7@9x�@9x�@9hs@9G�@8�`@8�@8A�@8 �@7�@7��@7\)@7�@7
=@6�@6��@6ff@6{@5��@5��@5O�@5V@4��@4z�@4Z@4(�@3��@333@2��@2�\@2=q@1�#@1hs@0�`@0�9@0�u@0r�@0bN@0bN@0A�@/�w@/\)@/;d@.��@.ff@-�@-��@-�@-`B@,��@,�@,�@,��@,(�@+��@+��@+��@+dZ@+C�@+o@*�H@*��@*��@*=q@*J@)��@)G�@(��@(�9@(bN@(A�@'��@'�@'|�@'+@&��@&��@&E�@&5?@&@%�-@%�h@%�@%V@$�D@$z�@$j@$I�@$9X@$�@$1@#��@#�m@#ƨ@#��@#S�@"��@"��@"�\@"M�@!�@!�^@!G�@!7L@!7L@ ��@ 1'@�@�;@�w@��@�P@;d@��@�R@��@�+@ff@�T@�-@�h@O�@?}@V@�@�@��@�j@�D@j@Z@9X@(�@�
@��@��@�@dZ@33@o@�@��@~�@M�@�@��@�@��@�^@��@��@��@�7@hs@&�@�`@Ĝ@�9@�9@�u@�u@r�@A�@ �@  @�@��@�@�P@\)@;d@+@
=@��@ff@E�@$�@�@�-@`B@��@�/@��@�j@z�@9X@(�@�@1@��@��@�@t�@dZ@"�@��@~�@n�@�@��@�@�#@��@x�@X@X@X@X@G�@G�@7L@%@�u@1'@�;@�;@�;@�w@�@�@��@l�@
=@�y@�R@��@E�@$�@�T@�-@`B@��@��@j@I�@1@1@��@ƨ@��@�@t�@S�@"�@
�@
�@
�H@
�@
�H@
��@
��@
��@
��@
��@
��@
�\@
~�@
M�@
=q@
=q@
-@
�@
J@	��@	�#@	��@	G�@	7L@��@��@��@�9@�@r�@Q�@1'@b@��@��@|�@l�@l�@l�@;d@�@V@E�@E�@5?@{@�@��@�h@�h@��@��@�j@��@z�@j@9X@�@�m@ƨ@ƨ@ƨ@ƨ@�F@�F@�F111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��7A��A��A�~�A�ZA� �A�=qA��uA�jA�  A�~�A�M�A�JA��#A��wA���A���A��A�l�A�E�A�(�A���A�~�A��A�ƨA���A��A�jA�G�A���A���A�|�A��A���A�jA���A�n�A�M�A�K�A�ƨA��A�dZA��HA��uA���A�E�A���A�G�A�&�A�oA��RA��A�K�A��/A��A�C�A�l�A�I�A�?}A���A���A��DA��A���A�z�A�dZA��\A�=qA��A���A�hsA��A�&�A��#A�E�A�$�A�/A��+A�O�A���A�r�A�oA�33A��^A���A�"�A~�`A|5?Az�Ax1'Av��Au�-At-Ar�/ArbNAq��Ap�An�Al�Aj=qAi�AgAc�hAbv�Aat�A`�uA`1'A_�A_oA^ffA\�!AYl�AW/AV�/AVv�AU��AT��ATM�AS��AS��ASK�AQ�-AO�AO�-AO�^AO�^AO�-AO��AOXAOoAN�ANv�AL^5AK�#AJ��AJjAJ �AH�AH-AF��AF{AE��AD�uACp�ACoAB�RAA��AA�A@�A?�;A>�!A>(�A=��A<-A;C�A:�uA9�^A8�RA8  A7�^A7x�A7
=A6A3x�A21'A17LA0A�A0  A/�^A/A.  A-hsA-A,r�A+��A+�A*{A(��A't�A&��A%�TA$^5A"^5A!�mA!�^A!hsA!;dA!&�A!
=A n�A�-A;dA+A�9A��A~�A1A��A?}A5?A�A�A��A�wAffA��A-A�^AS�A��A�#A
=A�`A��A�+A�^A�PAv�At�A
ffA1'A�yAI�A�
A?}A��A�;A"�A �A`BA n�@�M�@��h@��@��9@�r�@�1'@��
@��P@��@���@�@�`B@�(�@���@��@�E�@���@�&�@�F@��@�9X@���@�r�@�@�!@�E�@��@��@�V@�b@�+@��y@���@���@�r�@߮@ާ�@��@�S�@���@�`B@�z�@�n�@ԃ@ӍP@�"�@���@ҧ�@Ѳ-@�(�@�dZ@�^5@��@�r�@ʗ�@ɲ-@�/@�l�@���@�%@���@+@��-@�G�@��`@�A�@�o@��@�&�@��`@�Ĝ@�1@��@�S�@�^5@���@��u@��P@�`B@���@�z�@� �@��@��@�dZ@�"�@��y@���@��@��@�(�@���@��F@�{@��@���@�C�@�
=@�-@��@�Q�@�ff@��T@�p�@�G�@���@�j@��P@���@�J@�/@��9@�Q�@�ƨ@���@���@��+@�^5@�J@�?}@�j@�ƨ@�"�@�v�@�J@��^@���@�I�@���@�\)@�+@��\@�{@��T@�G�@��j@��@�l�@��R@�-@��#@���@���@���@�bN@�I�@�1'@��@��@�"�@���@�E�@�@��T@���@���@�7L@��`@�z�@�1'@��@��@���@��P@�dZ@��@��!@�n�@�$�@��^@�O�@��`@���@���@���@��j@���@�bN@�Q�@�I�@�(�@�b@��@���@���@�S�@�33@��@�
=@���@���@�E�@���@�x�@�&�@��/@���@���@���@���@�I�@�w@~ȴ@}�@|��@|�/@|I�@{t�@{@z��@zn�@zM�@z=q@z=q@zJ@yhs@x�`@x�@xQ�@xb@w�w@w�@v�@v�R@v��@vV@v@u�T@u�-@u�h@u�@up�@u`B@uO�@u/@t�@tI�@s��@s�
@sƨ@s��@s33@r�!@r^5@r=q@q��@qx�@q�@p��@p��@o��@o�w@o�w@o�@o�@pQ�@q%@qG�@pr�@pQ�@o�@o�w@o\)@mp�@l�@l��@l�j@lI�@k��@j��@jn�@jM�@j�@i�@i��@ihs@i7L@h��@hr�@g�w@g\)@f�R@fff@fE�@e@e��@e��@e��@e��@e�h@e/@e�G�O�@e`B@e/@d��G�O�@c�
@cdZ@b�@b��@b�!@b�\@b^5@b-@bJ@a��@a�7@aX@a%@`�@`Q�@`A�@`  @_�w@_K�@^��@^��@^E�@]�T@]p�@]V@\�@\I�@[ƨ@Z�@Zn�@Y�#@Y��@Yx�@Y&�@X�9@XbN@XA�@Xb@W�w@W\)@V�y@V�+@V$�@U��@U�h@U/@T��@Sƨ@S33@R��@RM�@Q�^@QG�@PĜ@PQ�@O�P@N�y@Nv�@M�T@M�-@M?}@Lz�@Lj@LZ@LI�@L1@K�
@K�F@K��@K"�@J��@J=q@I��@I�#G�O�@I�@H�9@H �@G��@G\)@GK�@GK�@G+@F�R@F��@F�+@F�+@F�+@F�+@Fff@Fff@F$�@E��@E�-@E��@E�hG�O�@D�@D�j@DI�@D�@C�
@C��@CC�@C"�@B��@B��@B�!G�O�@BJ@A7L@@�9@@bN@@1'@@b@?�;@?|�@>��@>��@>v�@>ff@>5?@>$�@>$�@>{@=�@=��@=��@=�-@=`B@=V@<�@<��@<�j@<��@<�jG�O�@<(�@;�m@;dZ@;"�@:�@:�H@:��@:�\@:=q@9��@9��@9��@9�7@9x�@9x�@9hs@9G�@8�`@8�@8A�@8 �@7�@7��@7\)@7�@7
=@6�@6��@6ff@6{@5��@5��@5O�@5V@4��@4z�@4Z@4(�@3��@333@2��@2�\@2=q@1�#@1hs@0�`@0�9@0�u@0r�@0bN@0bNG�O�@/�w@/\)@/;dG�O�@.ff@-�@-��@-�@-`B@,��@,�G�O�@,��@,(�@+��@+��@+��@+dZ@+C�@+o@*�H@*��@*��@*=q@*J@)��@)G�@(��@(�9@(bN@(A�@'��@'�@'|�@'+@&��@&��@&E�@&5?@&@%�-@%�h@%�@%V@$�D@$z�@$j@$I�@$9X@$�@$1@#��@#�m@#ƨ@#��@#S�@"��@"��@"�\@"M�@!�@!�^@!G�G�O�@!7L@ ��@ 1'@�@�;@�w@��@�P@;d@��@�R@��@�+@ff@�T@�-@�h@O�@?}@V@�@�@��@�j@�D@j@Z@9X@(�@�
@��@��@�@dZ@33@o@�@��@~�@M�@�@��@�@��@�^@��@��@��@�7@hs@&�@�`@Ĝ@�9@�9@�u@�u@r�@A�@ �@  @�@��@�@�P@\)@;d@+@
=@��@ff@E�@$�@�@�-@`B@��@�/@��@�j@z�@9X@(�@�@1@��@��@�@t�@dZ@"�@��@~�@n�@�@��@�@�#@��@x�@X@X@X@X@G�@G�@7L@%@�u@1'@�;@�;@�;@�w@�@�@��@l�@
=@�y@�R@��@E�@$�@�T@�-@`B@��@��@j@I�@1@1@��@ƨ@��@�@t�@S�@"�@
�@
�@
�H@
�@
�H@
��@
��@
��@
��@
��@
��@
�\@
~�@
M�@
=q@
=q@
-@
�@
J@	��@	�#@	��@	G�@	7L@��@��@��@�9@�@r�@Q�@1'@b@��@��@|�@l�@l�@l�@;d@�@V@E�@E�@5?@{@�@��@�hG�O�@��@��@�j@��@z�@j@9X@�@�m@ƨ@ƨ@ƨ@ƨ@�F@�F@�F111111111111111111111111111111111111131111111111111111111111111131111111111111111111111111111111111111111111131111111111311111111113111111111111111111111111111111111111111111111111111111113311111111111113111111111131111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111131111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141114111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111411111111111411111111111111111111111111141111111111111111111111111111111111111111111111111114111411111114111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111 B��B��B��B�\B|�BK�B�BJ�B�B��B�#B�B�B�B�B�B�B�B��B��BŢBŢB�wB�wB��B�}B�wB�RB�B��B�B��B�oBv�BT�BhsBe`BJ�B$�B\B��B	7B	7B��B�B�B�B�B�B�HB��B��BɺBÖBŢB�9B�^BŢB�dB�B��B�JB�JB|�BW
BM�B-B2-B �BJBBB
��B
�B
��B
�wB
��B
��B
��B
��B
��B
�=B
�B
�1B
u�B
_;B
C�B
J�B
(�B
6FB
'�B
#�B
�B
"�B
uB
B	�sB	��B	��B	ȴB	�LB	�=B	�B	��B	��B	��B	��B	�{B	�+B	m�B	VB	R�B	p�B	m�B	gmB	]/B	`BB	_;B	\)B	S�B	A�B	:^B	O�B	VB	S�B	R�B	O�B	J�B	E�B	C�B	7LB	�B	-B	)�B	&�B	&�B	�B	uB	VB	JB	bB	B��B	1B	B��B��B��B�B�NB�B�ZB�
B�
B�)B��B��B��B�B�
B��B�wB��B�B�3B�B�XB�FB�B��B��B��B��B��B�uB�VB}�B~�B~�B|�Bl�BdZBy�B�B~�B~�B}�Bv�BgmBJ�BjBq�BhsB[#BVBcTBdZB_;BT�BffBdZB`BBZBI�B7LBN�BO�BN�BG�BF�BE�BP�BL�BD�B33B�B'�B'�B!�BJB!�B(�B.B'�B%�B �B�B�B�B�B�B(�B-B0!B0!B0!B.B-B)�B%�B.B)�B"�B'�B%�B'�B+B'�B"�B�B!�B%�B�B+B)�B+B%�B�B�B&�B+B.B'�B%�B-B&�B"�B�B�B�B �B�B�B�B#�B)�B)�B&�B�B�B�B�B�B�B�B�B!�B�B�B#�B$�B&�B,B0!B0!B-B)�B-B0!B8RB8RB49B5?B5?B/B-B6FB2-B+BA�BB�BD�BF�BE�BD�BD�BB�B<jB49B#�B-BL�BN�BC�BF�BT�B\)B]/BXBXB]/BW
Bk�Bn�Bq�Bp�Bo�Bm�Br�Bu�By�B�B�B�B�B�VB�bB�VB�JB�1B�=B�\B�oB�{B��B��B��B��B��B��B��B��B�B�B�B�B�!B�LB�^B�wBĜBƨBĜB��B��B��B��B��B��B��B�B�#B�;B�HB�NB�NB�HB�`B�mB�B�B�B�B��B�B�B��B��B��B��B��B	B		7B	
=B	
=B	
=B	
=B	DB	bB	oB	hB	oB	uB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	$�B	(�B	,B	0!B	2-B	2-B	2-B	2-B	7LB	6FB	;dB	;dB	B�B	C�B	D�B	K�B	O�B	O�B	R�B	S�B	S�B	R�B	R�B	XB	[#B	_;B	`BB	bNB	cTB	hsB	jB	k�B	k�B	l�B	n�B	o�B	p�B	q�B	r�B	r�B	r�B	r�B	q�B	u�B	x�B	z�B	z�B	z�B	z�B	|�B	�B	�B	�B	�B	�+B	�JB	�VB	�VB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	�B	�!B	�!B	�'B	�'B	�'B	�-B	�-B	�-B	�-B	�FB	�LB	�^B	�qB	��B	��B	ÖB	B	B	B	��B	ĜB	��B	ɺB	��B	��B	��B	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�#B	�)B	�)B	�/B	�;B	�;B	�BB	�;B	�BB	�TB	�`B	�sB	�yB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B
B
B
B
B
B
B
B
B
B
%B
+B
	7B
1B
%B
1B
1B
1B
DB
JB
PB
JB
DB
PB
VB
VB
VB
VB
VB
PB
PB
PB
\B
\B
\B
VB
JB
\B
\B
bB
hB
oB
hB
uB
oB
{B
{B
uB
hB
bB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
!�B
 �B
�B
�B
�B
�B
!�B
#�B
#�B
#�B
#�B
"�B
#�B
%�B
%�B
&�B
&�B
&�B
&�B
%�B
$�B
%�B
&�B
'�B
'�B
&�B
'�B
(�B
)�B
(�B
(�B
)�B
)�B
)�B
+B
)�B
+B
+B
,B
-B
,B
+B
,B
-B
/B
/B
.B
/B
/B
2-B
33B
33B
33B
33B
2-B
0!B
1'B
33B
33B
1'B
49B
5?B
6FB
6FB
6FB
8RB
8RB
6FB
5?B
8RB
9XB
8RB
8RB
9XB
9XB
9XB
:^B
9XB
8RB
9XB
8RB
9XB
:^B
:^B
;dB
;dB
;dB
=qB
=qB
<jB
=qB
=qB
=qB
@�B
?}B
?}B
@�B
@�B
?}B
?}B
B�B
B�B
B�B
B�B
C�B
B�B
B�B
B�B
B�B
A�B
@�B
?}B
B�B
C�B
B�B
A�B
B�B
A�B
D�B
D�B
B�B
A�B
D�B
E�B
E�B
E�B
E�B
D�B
E�B
F�B
G�B
G�B
G�B
F�B
H�B
I�B
I�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
K�B
L�B
M�B
M�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
N�B
N�B
O�B
N�B
O�B
O�B
O�B
P�B
O�B
O�B
N�B
O�B
P�B
Q�B
Q�B
Q�B
Q�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
Q�B
R�B
S�B
R�B
R�B
R�B
S�B
S�B
S�B
R�B
S�B
S�B
VB
W
B
VB
VB
VB
W
B
XB
XB
W
B
W
B
XB
XB
XB
W
B
W
B
YB
ZB
YB
ZB
[#B
[#B
ZB
[#B
[#B
\)B
\)B
\)B
\)B
[#B
[#B
ZB
YB
[#B
\)B
^5B
^5B
^5B
^5B
^5B
^5B
]/B
\)B
^5B
^5B
^5B
^5B
_;B
^5B
_;B
_;B
_;B
`BB
`BB
aHB
bNB
cTB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
ffB
e`B
e`B
e`B
e`B
ffB
ffB
ffB
ffB
ffB
ffB
ffB
e`B
ffB
gmB
gmB
gmB
iyB
hsB
iyB
jB
jB
jB
jB
jB
jB
k�B
l�B
l�B
k�B
jB
iyB
iyB
l�B
m�B
l�B
l�B
l�B
k�B
l�B
l�B
jB
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B�HB~�BQ�B'mBR�B�oBңBیBڠBچB�eB�_B�1B�_B�SB�TB�.BƎB�tB�cB�.B��B��B��B��B�B��B��B��B��By�BX_Bi*Bf2BMjB(�BuB��B
=B
	B�wB�;B��B�]B��B�B�hB��B�B��BāB�YB��B��B��B�6B�/B��B��B��B~BY�BO�B/�B4B"�BBB�BtB
��B
�/B
͹B
�UB
��B
�/B
��B
��B
��B
�B
�[B
��B
w2B
abB
F�B
LdB
,B
7�B
)�B
%�B
7B
#�B
�B
B	�B	��B	�(B	ʌB	��B	�VB	�iB	�0B	�
B	�zB	�vB	��B	��B	p;B	Y�B	UMB	qB	n/B	hXB	^jB	`�B	_�B	\�B	T�B	C{B	<PB	O�B	VB	TB	SB	PB	K)B	FB	DB	8RB	!HB	-�B	+B	'�B	'�B	#B	�B	�B	PB	B	tB�<B	�B	�B�0B��B�xB��B�B�6B�`B��B�_B�B�2B�:B��BچB׍B��B�iB�*B��B�nB�;B��B��B�=B�$B��B��B��B��B��B��B� B��B�4B~BBn�Bf�Bz^B�uB}BcB~]BwfBh�BN<Bj�Bq�Bi_B\�BW�Bc�Bd�B`'BVmBf�Bd�B`�BZ�BK�B9�BO�BP�BO�BH�BG�BF�BQ4BM6BESB4�BpB)_B)_B#�B�B#:B)�B.�B(�B&�B!�B�B�B�B�BB)_B-�B0UB0oB0oB.�B-wB*�B&�B./B*B#�B(XB&�B(sB+�B(�B$B!-B"�B'B/B+�B*�B+�B&�B�BB'�B+�B.cB(�B&�B-�B'�B#�B �B�B �B!bB�BB�B$tB*KB*KB'mB�B�B vB�B�B vB�B vB"�B�B�B$�B%�B'�B,�B0�B0�B-�B*�B-�B0�B8�B8�B4�B5�B5�B/�B.B6�B3B,�BA�BB�BD�BF�BE�BEBD�BB�B=B5ZB&B.�BL�BO\BD�BG�BU�B\xB]�BX�BX�B]�BXEBk�Bo Bq�BqBp;BncBshBv`BzxB��B�{B��B��B��B��B��B��B��B��B��B�B��B�B�#B�KB�!B� B�TB�KB�mB��B�iB��B��B��B��B��B��B��B��B�B�B�B�B�&B�4B�NB�FB�mBیB�pB�|B�B�B��B�B��B��B��B��B��B��B��B�B��B�8B�$B�JB�BB	aB		7B	
rB	
rB	
XB	
rB	xB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	B	B	!B	%B	)*B	,=B	0;B	2-B	2GB	2|B	2�B	7�B	6�B	;�B	;�B	B�B	C�B	EB	LB	O�B	P.B	S&B	TB	TB	S&B	S@B	XEB	[qB	_VB	`vB	b�B	c�B	h�B	j�B	k�B	k�B	l�B	n�B	o�B	p�B	q�B	r�B	r�B	r�B	r�B	rB	vB	y	B	z�B	z�B	z�B	{B	}<B	�;B	�-B	�GB	�MB	�_B	�dB	��B	��B	��B	�{B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�,B	�bB	�$B	�"B	�/B	�WB	�QB	�eB	�5B	�UB	�UB	�AB	�AB	�[B	�GB	�|B	�aB	�|B	�zB	��B	�xB	��B	��B	��B	ðB	ªB	B	��B	��B	ĶB	ʦG�O�B	��B	��B	�G�O�B	�B	�B	��B	��B	� B	� B	�B	�B	�FB	�B	�B	�2B	�SB	�EB	�QB	�1B	�1B	�eB	�QB	�qB	�]B	�]B	�~B	�pB	�pB	�vB	ߤB	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	�B	�B	��B	� B	��B	��B	��B	�B	�B	��B	�B	�B	�B	�B	�"B	�"B	�B
 4B	�.B
B
B
3B
GB
3B
SB
SB
MB
MB
?B
EB
	RB
�G�O�B
fB
fB
�B
^B
dB
jB
~B
xB
jB
VB
VB
VB
pB
pB
�B
jB
�B
�B
�B
vB
�G�O�B
vB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�G�O�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
!�B
 �B
�G�O�B
�B
 B
!�B
#�B
$B
#�B
$B
#B
$B
&B
&B
'B
'B
&�B
'B
%�B
%,B
&B
'B
(
B
(
B
'B
(
B
)*B
)�B
)*B
)*B
*B
*B
*0B
+B
*B
+B
+B
,"B
-)B
,=B
+6B
,=B
-CB
/OB
/5B
.IB
/OB
/OB
2aB
3MB
3hB
3MB
3MB
2GG�O�B
1[B
3hB
3hG�O�B
4�B
5ZB
6zB
6zB
6zB
8lB
8lG�O�B
5�B
8lB
9rB
8�B
8lB
9rB
9�B
9rB
:xB
9�B
8�B
9�B
8�B
9�B
:�B
:xB
;�B
;B
;�B
=�B
=�B
<�B
=�B
=�B
=�B
@�B
?�B
?�B
@�B
@�B
?�B
?�B
B�B
B�B
B�B
B�B
C�B
B�B
B�B
B�B
B�B
A�B
@�B
?�B
B�B
C�B
B�B
A�B
B�B
A�B
D�G�O�B
B�B
A�B
D�B
E�B
E�B
E�B
E�B
D�B
E�B
F�B
G�B
G�B
G�B
F�B
H�B
I�B
I�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
MB
MB
K�B
MB
M�B
M�B
MB
MB
MB
L�B
L�B
M�B
NB
N�B
N�B
O�B
N�B
O�B
O�B
O�B
P�B
O�B
PB
OB
PB
Q B
Q�B
RB
R B
RB
QB
Q B
R B
RB
Q�B
R B
RB
SB
R B
SB
S�B
SB
S&B
SB
TB
TB
TB
S&B
TB
T,B
VB
W
B
VB
V9B
VB
W$B
XB
X+B
W$B
W$B
XB
XB
X+B
W?B
W?B
YKB
Z7B
Y1B
ZQB
[=B
[#B
Z7B
[=B
[WB
\)B
\CB
\CB
\)B
[#B
[=B
ZQB
YKB
[=B
\CB
^5B
^5B
^OB
^5B
^5B
^OB
]dB
\]B
^jB
^jB
^jB
^OB
_VB
^jB
_VB
_VB
_pB
`vB
`vB
a|B
b�B
cTB
bhB
bhB
bhB
cnB
cTB
cnB
cnB
dtB
e`B
ezB
e`B
ezB
e`B
ezB
ezB
ezB
ffB
e`B
e`B
e`B
e�B
ffB
ffB
ffB
f�B
ffB
f�B
f�B
ezB
f�B
g�B
g�B
g�B
iyB
h�B
i�B
jB
j�B
j�B
j�B
j�B
j�B
k�B
l�B
l�B
k�B
j�B
i�B
i�B
l�B
m�B
l�B
l�B
l�B
k�B
l�B
l�G�O�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�111113331111111111111111111111111111131111111111111111111111111131111111111111111111111111111111111111111111131111111111311111111113111111111111111111111111111111111111111111111111111111113311111111111113111111111131111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111131111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141114111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111411111111111411111111111111111111111111141111111111111111111111111111111111111111111111111114111411111114111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;oG�O�;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<*f�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.07(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201805240035002018052400350020180524003500201806221242102018062212421020180622124210201806042120422018060421204220180604212042  JA  ARFMdecpA19c                                                                20180520093518  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180520003524  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180520003527  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180520003527  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180520003528  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180520003528  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180520003528  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180520003528  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180520003530  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180520003530                      G�O�G�O�G�O�                JA  ARUP                                                                        20180520005707                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180520153752  CV  JULD            G�O�G�O�F��                JM  ARSQJMQC2.0                                                                 20180521000000  CF  TEMP_ADJUSTED_QCB�  D�� G�O�                JM  ARCAJMQC2.0                                                                 20180523153500  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180523153500  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180604122042  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20180605000000  CF  PSAL_ADJUSTED_QCA@  D�� G�O�                JM  ARCAJMTM1.0                                                                 20180622034210  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115131516                      G�O�G�O�G�O�                