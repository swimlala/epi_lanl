CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-05-23T00:35:14Z creation;2018-05-23T00:35:21Z conversion to V3.1;2019-12-19T07:41:44Z update;     
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
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p@   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t(   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �H   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �0   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �P   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ݈   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20180523003514  20200115131516  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0576_242                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�d����1   @�d�F)� @:$Ʌ�oi�dLH��1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @�  A   AffA@  A`  A�  A�  A�  A�  A���A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�33B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-�fD.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx�fDy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ DɃ3D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�<�DՀ D�� D�  D�@ Dր D�� D�  D�<�D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @{�@�@�AG�A>�HA^�HA~�HA�p�A�p�A�p�A�=qA�p�A�p�A�p�A�p�B�RB�RB�RB�RB'�RB/�RB7�RB?�RBG�RBO�RBW�RB_�RBg�RBo�RBw�RB�RB��)B��)B��)B��)B�\B��)B�\B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B�\B��)B��)B��)B�\B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
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
C��=C��
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
D {�D ��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D	{�D	��D
{�D
��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D {�D ��D!{�D!��D"{�D"��D#{�D#��D${�D$��D%{�D%��D&{�D&��D'{�D'��D({�D(��D){�D)��D*{�D*��D+{�D+��D,{�D,��D-��D-��D.{�D.��D/{�D/��D0{�D0��D1{�D1��D2{�D2��D3{�D3��D4{�D4��D5{�D5��D6{�D6��D7{�D7��D8{�D8��D9{�D9��D:{�D:��D;{�D;��D<{�D<��D={�D=��D>{�D>��D?{�D?��D@{�D@��DA{�DA��DB{�DB��DC{�DC��DD{�DD��DE{�DE��DF{�DF��DG{�DG��DH{�DH��DI{�DI��DJ{�DJ��DK{�DK��DL{�DL��DM{�DM��DN{�DN��DO{�DO��DP{�DP��DQ{�DQ��DR{�DR��DS{�DS��DT{�DT��DU{�DU��DV{�DV��DW{�DW��DX{�DX��DY{�DY��DZ{�DZ��D[{�D[��D\{�D\��D]{�D]��D^{�D^��D_{�D_��D`{�D`��Da{�Da��Db{�Db��Dc{�Dc��Dd{�Dd��De{�De��Df{�Df��Dg{�Dg��Dh{�Dh��Di{�Di��Dj{�Dj��Dk{�Dk��Dl{�Dl��Dm{�Dm��Dn{�Dn��Do{�Do��Dp{�Dp��Dq{�Dq��Dr{�Dr��Ds{�Ds��Dt{�Dt��Du{�Du��Dv{�Dv��Dw{�Dw��Dx��Dx��Dy{�Dy��Dz{�Dz��D{{�D{��D|{�D|��D}{�D}��D~{�D~��D{�D��D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D���D���D���D�@�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D½�D���D�=�D�}�Dý�D���D�=�D�}�DĽ�D���D�=�D�}�DŽ�D���D�=�D�}�Dƽ�D���D�=�D�}�Dǽ�D���D�=�D�}�DȽ�D���D�=�Dɀ�Dɽ�D���D�=�D�}�Dʽ�D���D�=�D�}�D˽�D���D�=�D�}�D̽�D���D�=�D�}�Dͽ�D���D�=�D�}�Dν�D���D�=�D�}�DϽ�D���D�=�D�}�Dн�D���D�=�D�}�Dѽ�D���D�=�D�}�Dҽ�D���D�=�D�}�Dӽ�D���D�=�D�}�DԽ�D���D�:�D�}�Dս�D���D�=�D�}�Dֽ�D���D�:�D�}�D׽�D���D�=�D�}�Dؽ�D���D�=�D�}�Dٽ�D���D�=�D�}�Dڽ�D���D�=�D�}�D۽�D���D�=�D�}�Dܽ�D���D�=�D�}�Dݽ�D���D�=�D�}�D޽�D���D�=�D�}�D߽�D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D���D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D� �111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�x�A�z�A�z�A��A��A��TA�C�A���A�v�A�oA���A�Q�A��\A���A�\)A� �A�ƨA�n�A�G�A�&�A��;A�z�A�;dA��HA��A�;dA���A�&�A���A���A�hsA��TA�dZA��mA�+A��PA�VA��A��wA��A�Q�A��A��\A��A�G�A�9XA���A�VA���A�-A��/A�C�A���A�ZA�+A�\)A��HA�l�A��\A��A�+A��A�;dA���A�{A��DA�-A��FA��A�ƨA��A���A��FA�\)A�K�A��#A�G�A��`A�%A�-A�/A|�A}%A{�PAy;dAv  AuXAu�At�RAs|�ArbNAql�Ap^5Ao�Ao�hAo%Am�Ak�-Ai|�Ah��Ag��Ag`BAf��Ae��AdVAb�yAbA�Aa��AaC�A`�HA_�-A^z�A\�A[�AZ��AZjAX�AXbAW�#AW�AU��AU\)AU+ATn�AS"�APv�AN�AL1'AK�PAJ�AH�AH �AG�hAF��AE�;AD�ADJAB��AB{AA�;AA��AAx�AA\)A@�/A@�A?|�A>��A>$�A=�TA<ffA:5?A9oA8�uA8�DA8jA8$�A7\)A6��A6M�A5��A5`BA5"�A5
=A4�9A3�A2v�A1�A1|�A1l�A0v�A.�A-��A,I�A*��A*�DA*$�A)�A(�`A'&�A%��A%&�A$^5A#C�A"��A"VA" �A"A!��A!XA!A E�A�AO�A�A �A��At�A&�AZA�
A�mAZA{A��A&�A��A��A{A��A�A33A9XA��A��A�PA�!A9XA7LA1A
JA�/A��Az�Ar�AVAVAK�AZA��A�-A�AhsAG�A"�A��AffA%@�n�@�\)@��+@���@��@�-@�\@�bN@��@�+@��`@띲@�R@�`B@�j@���@���@�dZ@�v�@噚@�-@��/@�j@���@��@�r�@�I�@��
@�t�@�G�@�^5@ղ-@�x�@�G�@�?}@�?}@�?}@���@���@�r�@�K�@҇+@��@ѩ�@�G�@���@�t�@�$�@���@�(�@��m@�t�@�
=@ʧ�@�/@Ǯ@���@Ų-@�O�@��@î@§�@��9@�K�@��y@�n�@��@��u@�1@��w@�S�@���@�@���@�ƨ@���@��+@�9X@���@��@���@���@�7L@���@��u@�I�@��@��y@���@�$�@���@��^@�x�@�z�@� �@�b@���@��
@�
=@���@�^5@��@���@�hs@�bN@���@�n�@��^@�O�@�bN@��@�"�@�M�@�x�@�I�@��w@��@�;d@��!@�n�@�-@��7@�Ĝ@�j@�1'@��P@��!@�ff@��7@���@�I�@��w@�l�@�K�@��\@��j@�bN@��@���@��@�33@��y@��R@�-@���@�V@��D@�j@�bN@�1'@�ƨ@���@�^5@��@�`B@��@���@��@�r�@�1'@��;@���@�|�@�l�@�K�@��@���@��R@�n�@�V@��@�@���@�X@���@�bN@���@�l�@�\)@�33@��@�
=@��@��y@��H@���@�n�@�E�@��@��^@��-@��@�&�@���@��@��`@��j@��D@�j@�(�@�P@\)@+@~��@~�R@~��@~�+@~�+@~v�@~E�@}�-@}p�@}V@|1@{�@{C�@{o@z�H@z�!@z��@zn�@z�@y��@yX@x�`@x�9@x�u@xbN@xQ�@xb@w��@w|�@w;d@v��@v�y@vȴ@v�+@vE�@v$�@v@u�h@t��@t�@t��@t(�@s�m@s��@st�@s"�@r��@r�\@r^5@rJ@q�#@qx�@q�@p �@oK�@n��@n�@n��@m�h@l��@lz�@lI�@l9X@k�
@k�@kC�@ko@ko@k@j�H@jn�@i�7@ix�@iX@i7L@h�9@h1'@h  @g�@gK�@f��@f5?@e�-@d��@d�D@d1@cƨ@c��@c��@c�@c�@ct�@cS�@co@b�@b��@b~�@bM�@b-@a�@a��@a�^@a��@a��@a�7@aG�@`�u@`�u@`�@_�@_�P@_\)@_;d@^ȴ@]�-@]�@]p�@]O�@\��@\�@\��@\�D@\z�@\I�@[�
@[C�@[o@[@Z~�@Z-@Z-@Y��@Y�^@Y��@Y�7@YG�@XbN@X  @W�@W�@W+@V�+@U�T@U��@U�@Up�@U/@T�@Tj@T1@S��@R��@Q��@Q�7@QX@Q�@P�9@P�@P1'@O�@O�;@O�@O|�@O;d@N�@NV@M@M��@Mp�@MV@L�@LZ@K��@KdZ@KdZ@KS�@J��@J~�@J�@I�@I��@I��@I7L@H�9@H�@HbN@HQ�@HA�@HA�@HA�@H1'@Hb@G�P@G�@F�R@F��@F��@F�+@Fff@Fff@FE�@F{@Ep�@E?}@D��@D�j@D�@Cƨ@Ct�@C33@C@B��@B~�@BM�@A��@A��@A�^@A�^@Ax�@A&�@@��@@Ĝ@@Ĝ@@�@@  @?�w@?�@?�P@?l�@?K�@?;d@>��@>v�@>V@>{@=�@=�-@=`B@<�/@<�D@<�@;��@;dZ@;"�@:��@:M�@:-@:J@9�#@9�^@9��@9hs@9%@8�`@8�`@8Ĝ@8��@8�u@8bN@81'@8b@7�w@7+@6�@6�+@6V@6{@5�@5@5p�@5/@4��@4Z@3�m@3�
@3�
@3�m@3�
@3�m@3�
@3ƨ@3��@3t�@3S�@3@2M�@1��@1x�@1hs@1hs@0�`@0r�@0A�@0 �@0  @/�;@/;d@.�y@.�@.�@.ȴ@.�+@-p�@-/@-V@,��@,�@,9X@+��@+�@+t�@+dZ@+o@*=q@*J@)�@)�^@)x�@)&�@(��@(�9@(�@'�w@'l�@';d@'�@&��@&�@&��@&�+@&ff@%�-@%/@$��@$�j@$�j@$��@$(�@#�m@#�F@#��@#dZ@#C�@#33@"�H@"~�@"�@"J@!�@!�7@!%@ Ĝ@ 1'@  �@�;@|�@�@�y@�@ȴ@��@v�@V@@��@��@��@��@p�@�@�@��@j@��@t�@C�@o@��@�\@~�@~�@n�@-@�#@�^@�^@�^@��@X@&�@��@�`@�u@Q�@Q�@ �@  @��@�@��@�P@|�@l�@l�@l�@+@�@�+@ff@5?@@@�@@�@`B@V@�@�j@z�@I�@9X@�@�@1@�
@��@dZ@33@o@@o@"�@�@�!@��@=q@-@�@�#@�#@�#@�#@��@��@�7@�7@hs@X@X@X@G�@�`@�u@r�@A�@b@��@��@|�@;d@
=@ȴ@�+@v�@V@�T@��@�@?}@�@�@�@Z@��@�
@ƨ@��@S�@"�@@
�@
��@
��@
~�@
n�@
M�@
-@
�@
J@	��@	�#@	��@	�^@	��@	��@	�7@	hs@	G�@	G�@	7L@	7L@	7L@��@Ĝ@�u@bN@bN@Q�@A�@A�@1'@b@�@��@;d@�y@v�@E�@{@@�h@p�@?}@/@��@�@��@��@z�@z�@I�@1@�
@ƨ@�F@��@�@�@S�@33@@��@��@��@�!@�!@��@~�@-@J@��@�@��@�7@x�@hs@X@G�144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�x�A�z�A�z�A��A��A��TA�C�A���A�v�A�oA���A�Q�A��\A���A�\)A� �A�ƨA�n�A�G�A�&�A��;A�z�A�;dA��HA��A�;dA���A�&�A���A���A�hsA��TA�dZA��mA�+A��PA�VA��A��wA��A�Q�A��A��\A��A�G�A�9XA���A�VA���A�-A��/A�C�A���A�ZA�+A�\)A��HA�l�A��\A��A�+A��A�;dA���A�{A��DA�-A��FA��A�ƨA��A���A��FA�\)A�K�A��#A�G�A��`A�%A�-A�/A|�A}%A{�PAy;dAv  AuXAu�At�RAs|�ArbNAql�Ap^5Ao�Ao�hAo%Am�Ak�-Ai|�Ah��Ag��Ag`BAf��Ae��AdVAb�yAbA�Aa��AaC�A`�HA_�-A^z�A\�A[�AZ��AZjAX�AXbAW�#AW�AU��AU\)AU+ATn�AS"�APv�AN�AL1'AK�PAJ�AH�AH �AG�hAF��AE�;AD�ADJAB��AB{AA�;AA��AAx�AA\)A@�/A@�A?|�A>��A>$�A=�TA<ffA:5?A9oA8�uA8�DA8jA8$�A7\)A6��A6M�A5��A5`BA5"�A5
=A4�9A3�A2v�A1�A1|�A1l�A0v�A.�A-��A,I�A*��A*�DA*$�A)�A(�`A'&�A%��A%&�A$^5A#C�A"��A"VA" �A"A!��A!XA!A E�A�AO�A�A �A��At�A&�AZA�
A�mAZA{A��A&�A��A��A{A��A�A33A9XA��A��A�PA�!A9XA7LA1A
JA�/A��Az�Ar�AVAVAK�AZA��A�-A�AhsAG�A"�A��AffA%@�n�@�\)@��+@���@��@�-@�\@�bN@��@�+@��`@띲@�R@�`B@�j@���@���@�dZ@�v�@噚@�-@��/@�j@���@��@�r�@�I�@��
@�t�@�G�@�^5@ղ-@�x�@�G�@�?}@�?}@�?}@���@���@�r�@�K�@҇+@��@ѩ�@�G�@���@�t�@�$�@���@�(�@��m@�t�@�
=@ʧ�@�/@Ǯ@���@Ų-@�O�@��@î@§�@��9@�K�@��y@�n�@��@��u@�1@��w@�S�@���@�@���@�ƨ@���@��+@�9X@���@��@���@���@�7L@���@��u@�I�@��@��y@���@�$�@���@��^@�x�@�z�@� �@�b@���@��
@�
=@���@�^5@��@���@�hs@�bN@���@�n�@��^@�O�@�bN@��@�"�@�M�@�x�@�I�@��w@��@�;d@��!@�n�@�-@��7@�Ĝ@�j@�1'@��P@��!@�ff@��7@���@�I�@��w@�l�@�K�@��\@��j@�bN@��@���@��@�33@��y@��R@�-@���@�V@��D@�j@�bN@�1'@�ƨ@���@�^5@��@�`B@��@���@��@�r�@�1'@��;@���@�|�@�l�@�K�@��@���@��R@�n�@�V@��@�@���@�X@���@�bN@���@�l�@�\)@�33@��@�
=@��@��y@��H@���@�n�@�E�@��@��^@��-@��@�&�@���@��@��`@��j@��D@�j@�(�@�P@\)@+@~��@~�R@~��@~�+@~�+@~v�@~E�@}�-@}p�@}V@|1@{�@{C�@{o@z�H@z�!@z��@zn�@z�@y��@yX@x�`@x�9@x�u@xbN@xQ�@xb@w��@w|�@w;d@v��@v�y@vȴ@v�+@vE�@v$�@v@u�h@t��@t�@t��@t(�@s�m@s��@st�@s"�@r��@r�\@r^5@rJ@q�#@qx�@q�@p �@oK�@n��@n�@n��@m�h@l��@lz�@lI�@l9X@k�
@k�@kC�@ko@ko@k@j�H@jn�@i�7@ix�@iX@i7L@h�9@h1'@h  @g�@gK�@f��@f5?@e�-@d��@d�D@d1@cƨ@c��@c��@c�@c�@ct�@cS�@co@b�@b��@b~�@bM�@b-@a�@a��@a�^@a��@a��@a�7@aG�@`�u@`�u@`�@_�@_�P@_\)@_;d@^ȴ@]�-@]�@]p�@]O�@\��@\�@\��@\�D@\z�@\I�@[�
@[C�@[o@[@Z~�@Z-@Z-@Y��@Y�^@Y��@Y�7@YG�@XbN@X  @W�@W�@W+@V�+@U�T@U��@U�@Up�@U/@T�@Tj@T1@S��@R��@Q��@Q�7@QX@Q�@P�9@P�@P1'@O�@O�;@O�@O|�@O;d@N�@NV@M@M��@Mp�@MV@L�@LZ@K��@KdZ@KdZ@KS�@J��@J~�@J�@I�@I��@I��@I7L@H�9@H�@HbN@HQ�@HA�@HA�@HA�@H1'@Hb@G�P@G�@F�R@F��@F��@F�+@Fff@Fff@FE�@F{@Ep�@E?}@D��@D�j@D�@Cƨ@Ct�@C33@C@B��@B~�@BM�@A��@A��@A�^@A�^@Ax�@A&�@@��@@Ĝ@@Ĝ@@�@@  @?�w@?�@?�P@?l�@?K�@?;d@>��@>v�@>V@>{@=�@=�-@=`B@<�/@<�D@<�@;��@;dZ@;"�@:��@:M�@:-@:J@9�#@9�^@9��@9hs@9%@8�`@8�`@8Ĝ@8��@8�u@8bN@81'@8b@7�w@7+@6�@6�+@6V@6{@5�@5@5p�@5/@4��@4Z@3�m@3�
@3�
@3�m@3�
@3�m@3�
@3ƨ@3��@3t�@3S�@3@2M�@1��@1x�@1hs@1hs@0�`@0r�@0A�@0 �@0  @/�;@/;d@.�y@.�@.�@.ȴ@.�+@-p�@-/@-V@,��@,�@,9X@+��@+�@+t�@+dZ@+o@*=q@*J@)�@)�^@)x�@)&�@(��@(�9@(�@'�w@'l�@';d@'�@&��@&�@&��@&�+@&ff@%�-@%/@$��@$�j@$�j@$��@$(�@#�m@#�F@#��@#dZ@#C�@#33@"�H@"~�@"�@"J@!�@!�7@!%@ Ĝ@ 1'@  �@�;@|�@�@�y@�@ȴ@��@v�@V@@��@��@��@��@p�@�@�@��@j@��@t�@C�@o@��@�\@~�@~�@n�@-@�#@�^@�^@�^@��@X@&�@��@�`@�u@Q�@Q�@ �@  @��@�@��@�P@|�@l�@l�@l�@+@�@�+@ff@5?@@@�@@�@`B@V@�@�j@z�@I�@9X@�@�@1@�
@��@dZ@33@o@@o@"�@�@�!@��@=q@-@�@�#@�#@�#@�#@��@��@�7@�7@hs@X@X@X@G�@�`@�u@r�@A�@b@��@��@|�@;d@
=@ȴ@�+@v�@V@�T@��@�@?}@�@�@�@Z@��@�
@ƨ@��@S�@"�@@
�@
��@
��@
~�@
n�@
M�@
-@
�@
J@	��@	�#@	��@	�^@	��@	��@	�7@	hs@	G�@	G�@	7L@	7L@	7L@��@Ĝ@�u@bN@bN@Q�@A�@A�@1'@b@�@��@;d@�y@v�@E�@{@@�h@p�@?}@/@��@�@��@��@z�@z�@I�@1@�
@ƨ@�F@��@�@�@S�@33@@��@��@��@�!@�!@��@~�@-@J@��@�@��@�7@x�@hs@X@G�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��BdZB2-B�-BɺB�
B�B�
B��B��BŢB�qB��B��B��B��B��B��B��BȴBŢB�}B�XB�'B��B��B�hB�B� B{�Br�Bn�B]/B^5BYB`BB\)BQ�BM�B<jB/B1BJB��B��B��B�B�sB�sB�/B�#BǮB�RB��B�B��B�VB�B� Bz�BcTB=qBF�BA�B�B+B!�B�B�BB
�TB
��B
ƨB
�B
�RB
�'B
��B
r�B
n�B
t�B
[#B
J�B
>wB
�B
;dB
:^B
49B
"�B
�B
�B
oB
�B
\B
B	�B	��B	��B	�)B	�/B	�/B	��B	ĜB	�}B	�3B	�qB	�dB	�XB	�3B	��B	��B	�JB	�hB	�+B	�7B	r�B	�B	�B	x�B	m�B	m�B	n�B	]/B	E�B	&�B	�B	VB	�B	�B	JB	{B	bB		7B	%B��B	  B��B	B	+B	B	B��B��B�B�B�NB�NB�/BƨB�9BȴB��B�B�B��BŢB��BƨBĜBBĜBÖB�dB�!B��B�-B�!B�!B��B�JB��B�uB�JB��B��B��B�bB|�Bx�B�+B�Bz�B�B�B�+B�%B�Bz�Bz�Bq�Bt�Bp�Bk�Bm�Bn�Bl�BffB]/BQ�BB�B>wB\)B\)BR�BW
BT�BJ�B;dB;dBA�B7LB<jB2-B6FB-B8RB$�B �B{B"�B9XB:^B7LB-B�B	7B%�B1'B5?B6FB7LB49B0!B)�B�B	7BBVB%�B"�B�BDB+B�B#�B(�B"�B#�B'�B&�B-B-B1'B-B#�B �BPB!�B1'B0!B/B+B#�B�B��BBbB$�B/B/B1'B1'B0!B,B,B&�B!�B"�B)�B'�B'�B#�B�B�B�B$�B+B&�B&�B#�B�B�B$�B%�B-B,B#�B"�B!�B+B;dB<jB9XB:^BB�BF�BE�BD�BA�B>wBB�BI�BA�B33B8RB:^BA�BH�BP�BW
BVBVBR�BR�BYBZB\)B_;B\)BXB`BBe`BdZBcTB^5BbNBe`Be`BdZBaHB\)B_;B\)Be`Bl�BhsBo�Br�Bp�Bt�Bv�B�B�7B�1B�+B�JB�PB�7B�PB�uB��B�uB�hB��B��B��B��B��B��B�B��B��B�FB�RB�jB�}B�wB�}B��B�}BBǮB��B��B��B��B��B��B�B�#B�;B�fB�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B	B	  B	B	B	+B	\B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	"�B	!�B	!�B	&�B	)�B	)�B	(�B	)�B	,B	-B	/B	7LB	8RB	9XB	;dB	>wB	@�B	@�B	@�B	?}B	?}B	C�B	E�B	E�B	L�B	P�B	R�B	S�B	T�B	VB	VB	W
B	W
B	XB	[#B	_;B	aHB	bNB	cTB	cTB	dZB	e`B	gmB	hsB	k�B	k�B	k�B	m�B	n�B	o�B	o�B	q�B	v�B	v�B	u�B	y�B	}�B	� B	� B	�B	�B	�B	�B	�+B	�+B	�+B	�+B	�JB	�oB	��B	��B	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�-B	�-B	�3B	�9B	�RB	�XB	�^B	�}B	��B	ĜB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�#B	�#B	�B	�B	�BB	�HB	�BB	�;B	�TB	�TB	�TB	�TB	�NB	�HB	�NB	�`B	�fB	�`B	�fB	�yB	�sB	�sB	�yB	�sB	�mB	�`B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B	��B
  B
B
B
B
%B
B
B
B
B
+B
1B
1B
+B
1B
DB
DB
JB
PB
PB
JB
JB
DB
	7B
DB
VB
\B
bB
bB
\B
bB
\B
\B
PB
bB
bB
hB
bB
oB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
 �B
$�B
#�B
$�B
$�B
$�B
$�B
$�B
&�B
&�B
&�B
&�B
&�B
%�B
%�B
%�B
$�B
$�B
&�B
'�B
(�B
(�B
)�B
)�B
(�B
)�B
)�B
)�B
,B
/B
/B
/B
/B
/B
/B
.B
.B
.B
.B
-B
+B
-B
0!B
2-B
1'B
/B
0!B
33B
33B
33B
33B
1'B
33B
5?B
5?B
5?B
33B
1'B
5?B
7LB
7LB
6FB
5?B
5?B
9XB
9XB
9XB
7LB
6FB
:^B
;dB
;dB
;dB
:^B
;dB
;dB
:^B
9XB
<jB
>wB
>wB
>wB
>wB
=qB
>wB
=qB
;dB
<jB
?}B
?}B
A�B
@�B
?}B
@�B
B�B
B�B
B�B
B�B
C�B
B�B
A�B
B�B
D�B
D�B
B�B
B�B
D�B
C�B
E�B
D�B
D�B
E�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
J�B
J�B
J�B
H�B
H�B
J�B
J�B
I�B
I�B
J�B
L�B
L�B
L�B
M�B
N�B
N�B
N�B
M�B
M�B
O�B
P�B
P�B
O�B
N�B
O�B
O�B
P�B
P�B
P�B
R�B
Q�B
Q�B
Q�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
R�B
Q�B
R�B
T�B
T�B
T�B
VB
VB
T�B
T�B
T�B
T�B
VB
VB
VB
VB
XB
XB
XB
XB
W
B
W
B
XB
YB
ZB
ZB
[#B
[#B
ZB
ZB
ZB
ZB
[#B
[#B
]/B
]/B
]/B
]/B
\)B
]/B
]/B
]/B
]/B
]/B
^5B
]/B
]/B
[#B
]/B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
`BB
`BB
aHB
aHB
`BB
`BB
bNB
bNB
bNB
bNB
bNB
bNB
cTB
e`B
e`B
e`B
dZB
e`B
ffB
ffB
ffB
ffB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
hsB
hsB
iyB
jB
jB
iyB
iyB
hsB
iyB
iyB
iyB
k�B
jB
k�B
k�B
jB
jB
iyB
iyB
hsB
iyB
iyB
iyB
jB
iyB
k�B
k�B
k�B
l�B
l�B
m�B
m�B
m�B
m�B
n�B
m�B
m�B
n�B
n�B
o�B
o�B
o�B
o�B
n�B
n�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
o�B
o�B
p�B
p�B
q�B
p�B
p�B
q�B
r�B
r�B
r�B
s�144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B�$Bj�B;�B�?B�B�B��B��B��B��B�zB�cB�}BΊBңBЗB�aB�uBΊBɆB�?B�OB�B�aB�TB��B��B��B�B}"Bs�Bo�B_B_�BZB`�B\�BR�BNVB=�B0�B�B�B;B�<B��B�9B�B�_B޸B��BɆB��B��B�/B�B�bB��B� B{�Be`B@�BHBB�B"�B,B#:B �B�B�B
�B
�B
�7B
�vB
��B
��B
�)B
v�B
p�B
v`B
^B
MB
AUB
!bB
<B
:�B
5B
$tB
B
�B
�B

B
�B
3B	�hB	��B	�}B	�/B	�OB	ݲB	�B	�%B	�;B	��B	�(B	�6B	��B	�B	��B	�QB	�<B	��B	��B	�#B	t�B	��B	��B	y�B	o B	ncB	o5B	^�B	G�B	*KB	 �B	NB	 �B	?B	�B	�B	NB	
�B	+B�BB	;B�zB	�B	zB	mB	aB�cB��B�B�qB�nB� B�B��B��B��B�{B�7B�SB�vB��B��B�EB�SB�-B��B��B�B�[B��B��B��B��B�NB�B�B��B�VB��B�pB��B��BcBz�B��B�AB|PB��B��B�zB�tB��B{�B{Br�BuZBq�Bl�BnIBoBmBgB^jBS@BEB@�B\xB\�BS�BW�BU�BK�B="B<�BB�B8�B=�B3�B7�B.�B9$B&fB"�B
B$@B9�B:�B7�B-�B�B^B&�B1�B5�B6�B7�B4�B0�B*�B�B^BzBB&�B#�B�B6B	lB�B$�B)�B$B$�B(�B'�B-�B-�B1�B-�B$�B!�B(B"�B1AB0UB/iB+QB$@BYB��B�B B%FB/5B/OB1AB1[B0;B,qB,qB'mB"�B#�B*B(sB(sB$tB�B�B�B%zB+6B'RB'mB$ZB�B�B%�B&�B-]B,�B$�B#�B#TB,"B;�B<�B:B;BCBF�BF%BE9BBAB?cBCGBJ=BB�B5%B9�B;�BBuBIlBQhBWYBVmBVSBS�BS�BYeBZ�B\�B_pB\�BX�B`vBezBd�Bc�B^�Bb�Be�Be�Bd�Ba�B]B_�B]/Be�Bl�BiDBpUBsMBqvButBw�B��B�lB��B��B��B��B��B��B��B��B��B�:B��B�?B�IB�2B�RB�KB�WB��B�'B�zB��B��B��B��B��B��B� B�-B�1B�0B�B�B�:B�\BϑBևBۦBߤB��B�B�B��B��B�B��B��B��B�B�B�B�B�	B�B�B�6B	;B	 OB	oB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	"�B	"B	"B	'B	*0B	*0B	)*B	*0B	,WB	-wB	/�B	7�B	8lB	9rB	;B	>�B	@�B	@�B	@�B	?�B	?�B	C�B	E�B	FB	MB	Q B	SB	TB	U2B	VB	VB	WYB	W?B	XEB	[qB	_VB	a|B	bhB	c�B	cnB	dtB	e�B	g�B	h�B	k�B	k�B	k�B	m�B	n�B	o�B	o�B	q�B	v�B	v�B	vB	y�B	~B	�4B	�OB	�UB	�MB	�3B	�mB	�EB	�_B	�_B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�B	�
B	�
B	�B	�B	�2B	�B	�)B	�"B	�6B	�CB	�GB	�aB	�hB	��B	��B	��B	��B	��B	��B	ĶB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�B	�B	� B	�B	�@B	�TB	�+B	�+B	�?B	�KB	�=B	�=B	�KB	�yB	�BB	�HB	�vB	�pB	�nB	�B	�B	�nB	�B	�|B	�B	�zB	�B	�B	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�!B	�B	��B	��B	��B	�B	�B	��B	�B	�B	�B	�B	�B	�6B	�B	�"B
 B
 B	�.B
 4B
;B
UB
3B
?B
SB
aB
9B
SB
_B
KB
KB
zB
fB
^B
^B
JB
PB
PB
JB
~B
^B
	lB
xB
pB
\B
bB
}B
vB
bB
vB
vB
�B
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
 �B
$�B
#�B
$�B
$�B
$�B
%B
%B
'B
&�B
'B
'B
'B
&B
&B
%�B
%B
%B
'B
(
B
)*B
)*B
*B
*B
)*B
*B
*0B
*0B
,=B
/5B
/B
/B
/B
/B
/B
./B
./B
./B
./B
-]B
+kB
-CB
0;B
2-B
1[B
/OB
0UB
3MB
3MB
3hB
3hB
1vB
3MB
5?B
5?B
5ZB
3hB
1�B
5tB
7�B
7fB
6zB
5tB
5tB
9rB
9rB
9rB
7�B
6�B
:xB
;B
;B
;B
:xB
;B
;B
:xB
9�B
<�B
>�B
>�B
>�B
>�B
=�B
>�B
=�B
;�B
<�B
?�B
?�B
A�B
@�B
?�B
@�B
B�B
B�B
B�B
B�B
C�B
B�B
A�B
B�B
D�B
D�B
B�B
B�B
D�B
C�B
E�B
D�B
D�B
E�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
J�B
J�B
J�B
H�B
H�B
J�B
J�B
I�B
I�B
KB
MB
MB
MB
M�B
N�B
N�B
N�B
NB
M�B
O�B
P�B
P�B
O�B
OB
O�B
O�B
QB
Q B
QB
R�B
RB
RB
RB
SB
S�B
S�B
S�B
S�B
S�B
TB
S&B
R B
SB
UB
UB
UB
VB
VB
UB
UB
UB
UB
V9B
V9B
V9B
VB
XB
XEB
X+B
XEB
W$B
W$B
X+B
YKB
Z7B
Z7B
[#B
[#B
ZQB
ZQB
Z7B
ZQB
[=B
[=B
]IB
]/B
]/B
]IB
\CB
]IB
]/B
]IB
]dB
]/B
^5B
]/B
]IB
[WB
]IB
^OB
^OB
^OB
^OB
_VB
_VB
_VB
_VB
`vB
`\B
aHB
a|B
`vB
`\B
bhB
b�B
bhB
b�B
b�B
bhB
c�B
e`B
ezB
ezB
d�B
ezB
f�B
f�B
f�B
f�B
g�B
gmB
g�B
g�B
hsB
h�B
hsB
h�B
hsB
hsB
iyB
iyB
h�B
h�B
i�B
j�B
j�B
iyB
iyB
h�B
i�B
i�B
i�B
k�B
j�B
k�B
k�B
j�B
j�B
i�B
i�B
h�B
i�B
i�B
i�B
j�B
i�B
k�B
k�B
k�B
l�B
l�B
m�B
m�B
m�B
m�B
n�B
m�B
m�B
n�B
n�B
o�B
o�B
o�B
o�B
n�B
n�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
o�B
o�B
p�B
p�B
q�B
p�B
p�B
q�B
r�B
r�B
r�B
s�133111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.07(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201805270036122018052700361220180527003612201806221242192018062212421920180622124219201806042120522018060421205220180604212052  JA  ARFMdecpA19c                                                                20180523093508  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180523003514  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180523003517  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180523003518  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180523003519  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180523003519  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180523003519  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcp2.8e                                                                20180523003519  QCF$                G�O�G�O�G�O�            4000JA  ARGQaqcp2.8e                                                                20180523003519  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180523003519  QCF$                G�O�G�O�G�O�            4000JA  ARGQrqcpt16c                                                                20180523003520  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180523003521                      G�O�G�O�G�O�                JA  ARUP                                                                        20180523005728                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180523153357  CV  JULD            G�O�G�O�F�%�                JM  ARGQJMQC2.0                                                                 20180523153357  CV  JULD_LOCATION   G�O�G�O�F�%�                JM  ARGQJMQC2.0                                                                 20180523153357  CV  LATITUDE        G�O�G�O�A� �                JM  ARGQJMQC2.0                                                                 20180523153357  CV  LONGITUDE       G�O�G�O��"a�                JM  ARCAJMQC2.0                                                                 20180526153612  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180526153612  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180604122052  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20180605000000  CF  PSAL_ADJUSTED_QC@�  A   G�O�                JM  ARSQJMQC2.0                                                                 20180605000000  CF  TEMP_ADJUSTED_QC@�  A   G�O�                JM  ARCAJMTM1.0                                                                 20180622034219  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115131516                      G�O�G�O�G�O�                