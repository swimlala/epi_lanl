CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-08-21T00:35:14Z creation;2018-08-21T00:35:19Z conversion to V3.1;2019-12-19T07:34:38Z update;     
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
_FillValue                 �  I0   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p(   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ͬ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �@   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �P   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �T   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �X   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �\   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �`   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180821003514  20200115131518  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              A   JA  I2_0576_272                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�{68�1   @�{6�}( @9����m�d`�j~��1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  BffB ffB'��B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C�fC  C
  C�C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C[�fC^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>�fD?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DK��DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR�fDS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Diy�Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�FfD�c31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@�@�A�HA>�HA^�HA~�HA�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�B�RB�RB�B �B'Q�B/�RB7�RB?�RBG�RBO�RBW�RB_�RBg�RBo�RBw�RB�RB��)B��)B���B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)C�C�C�{C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�{C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
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
D {�D ��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D	{�D	��D
{�D
��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D {�D ��D!{�D!��D"{�D"��D#{�D#��D${�D$��D%{�D%��D&{�D&��D'{�D'��D({�D(��D){�D)��D*{�D*��D+{�D+��D,{�D,��D-{�D-��D.{�D.��D/{�D/��D0{�D0��D1{�D1��D2{�D2��D3{�D3��D4{�D4��D5{�D5��D6{�D6��D7{�D7��D8{�D8��D9{�D9��D:{�D:��D;{�D;��D<{�D<��D={�D=��D>��D>��D?{�D?��D@{�D@��DA{�DA��DB{�DB��DC{�DC��DD{�DD��DE{�DE��DF{�DF��DG{�DG��DH{�DH��DI{�DI��DJ{�DJ��DK{�DK�DL{�DL��DM{�DM��DN{�DN��DO{�DO��DP{�DP��DQ{�DQ��DR��DR��DS{�DS��DT{�DT��DU{�DU��DV{�DV��DW{�DW��DX{�DX��DY{�DY��DZ{�DZ��D[{�D[��D\{�D\��D]{�D]��D^{�D^��D_{�D_��D`{�D`��Da{�Da��Db{�Db��Dc{�Dc��Dd{�Dd��De{�De��Df{�Df��Dg{�Dg��Dh{�Dh��DiuDi��Dj{�Dj��Dk{�Dk��Dl{�Dl��Dm{�Dm��Dn{�Dn��Do{�Do��Dp{�Dp��Dq{�Dq��Dr{�Dr��Ds{�Ds��Dt{�Dt��Du{�Du��Dv{�Dv��Dw{�Dw��Dx{�Dx��Dy{�Dy��Dz{�Dz��D{{�D{��D|{�D|��D}{�D}��D~{�D~��D{�D��D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D½�D���D�=�D�}�Dý�D���D�=�D�}�DĽ�D���D�=�D�}�DŽ�D���D�=�D�}�Dƽ�D���D�=�D�}�Dǽ�D���D�=�D�}�DȽ�D���D�=�D�}�Dɽ�D���D�=�D�}�Dʽ�D���D�=�D�}�D˽�D���D�=�D�}�D̽�D���D�=�D�}�Dͽ�D���D�=�D�}�Dν�D���D�=�D�}�DϽ�D���D�=�D�}�Dн�D���D�=�D�}�Dѽ�D���D�=�D�}�Dҽ�D���D�=�D�}�Dӽ�D���D�=�D�}�DԽ�D���D�=�D�}�Dս�D���D�=�D�}�Dֽ�D���D�=�D�}�D׽�D���D�=�D�}�Dؽ�D���D�=�D�}�Dٽ�D���D�=�D�}�Dڽ�D���D�=�D�}�D۽�D���D�=�D�}�Dܽ�D���D�=�D�}�Dݽ�D���D�=�D�}�D޽�D���D�=�D�}�D߽�D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�z�D��D���D�:�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D���D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D� �D�D)D�`�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A���A�ĜA֛�A։7A�=qAѓuAк^A�E�A���AϾwA�M�AΟ�A�t�A�ffA�XA�/Aͣ�A̗�A�l�AƩ�A���A�z�A��A��A��PA���A��A���A�\)A�l�A���A�\)A�VA�^5A�C�A��A�K�A�dZA� �A��TA�A�$�A�A�A�7LA��A�"�A��A�O�A�+A��#A��A��yA�A�33A�A�ffA�n�A���A�$�A��/A��A�I�A��wA��7A�ffA�ĜA�ĜA�A�\)A��A�M�A��A�p�A�`BA�1'A�l�A���A���A���A���A�{A�A�|�A���A��yA��A��A�K�A��DA���A�1'A��A�VA�A"�A}��A|M�Az��AwAv1'As?}Arz�Aq��Ap��Ao��Am�
AlI�AjȴAi��AhI�Ag��Af�jAe�mAdbNAct�Ab(�A`�HA`�DA`=qA^�A\M�A[;dA[VAZ�HAZM�AV{AT�AS�mASVARbAP��API�AP(�APbAO�hANbALM�AK|�AJĜAI�-AH�AHbAG`BAFn�AEXAD5?AC�ABE�AA��AAO�A@�A?�7A>�\A=��A=+A<�DA:�A9VA8�A8M�A7�PA733A6��A6��A6bNA5�;A5t�A5VA4{A2=qA1|�A1dZA1;dA0E�A/��A.��A.z�A-�7A,A�A+K�A*�uA*VA*bA)�A(  A&�/A&�DA&VA%S�A$  A#dZA"�\A!p�A -A�PA;dA�+A�PA�AffA�mA��AO�A�A(�At�A5?A�FA7LA�AjA��AK�A
=Av�AA�hA�A|�A"�A��A$�A��A��A�A�Ax�A
��A
A�A	��A	?}A�yA^5AhsA�RA�mA�Az�AAK�Av�At�A J@�E�@��@���@�1'@��m@���@�Ĝ@��@�h@�bN@��m@�@���@웦@�C�@�!@��@�D@�^5@�@��@�@�  @��y@݉7@��@��
@��H@��@ٙ�@أ�@�1'@ו�@�~�@�/@�S�@�@�bN@��@�%@˶F@ʰ!@�?}@�z�@��;@���@��T@���@Ĵ9@�
=@�=q@��@���@��u@� �@�dZ@�^5@��@��#@���@�&�@���@�j@�A�@��F@��H@��@�&�@�r�@�
=@�G�@���@�j@��F@�@��@�7L@�ȴ@�O�@�%@��/@���@� �@��@�ȴ@��\@��@�?}@�V@�Ĝ@��@��@��y@��H@��R@��@�hs@��@��@�r�@�j@�bN@�b@��y@��R@�@�&�@���@���@���@��-@��7@��@�G�@���@���@��@��@��+@�M�@��h@� �@��@�+@���@���@��-@�V@���@�Q�@�1'@��@��@�t�@�v�@���@�G�@��@��9@��u@�j@� �@���@��@���@�dZ@���@�~�@��@�X@��@��@�Ĝ@��9@��u@�z�@�b@�1@��m@��@�C�@�~�@��@��h@��@�p�@�X@��9@���@�ƨ@���@�l�@�dZ@�\)@�C�@���@���@�E�@��-@�O�@�&�@���@�I�@�(�@��
@�;d@��y@���@���@���@��\@��+@�~�@�ff@�$�@��-@���@�x�@�O�@��@��/@��9@��u@�9X@��@\)@~V@}�@}?}@}�@|��@|�j@|�D@|Z@|(�@{�
@{�@z�!@y�@y�^@yG�@y&�@y�@y&�@y7L@xbN@xQ�@x  @wl�@w+@w
=@v�y@v�@vȴ@v��@vV@v@u�@tZ@t�@s�
@so@r=q@q��@qG�@p��@p�u@pA�@pb@o��@o�@ol�@n��@n�+@nE�@m�@m��@m�h@mO�@l��@l�j@lj@k�m@kS�@k"�@ko@j�\@j�\@j~�@jn�@j=q@i��@i�^@i��@i�7@i�7@ihs@hr�@hA�@g�@g�P@gl�@gl�@g
=@fȴ@fv�@e��@e�-@e�@d�/@d9X@c��@c�
@c�F@c��@ct�@cC�@b��@bn�@bM�@bJ@ax�@a7L@a&�@a%@`b@_�P@^ȴ@^�+@^v�@^ff@^V@]p�@\��@\��@\j@\�@[�m@[��@Z�@Z��@Z=q@Y�^@Yx�@Yhs@Yhs@YX@X��@X�@X1'@W�w@W|�@W;d@V5?@U��@U�@T�/@T�@T9X@SC�@S@R�\@RM�@Q�#@Q��@Qhs@Q�@P�u@O�;@OK�@O+@N�R@N5?@M��@Mp�@M�@L�/@L��@K��@KS�@K"�@J��@J=q@I�#@I�^@I��@Ihs@I7L@H��@H �@G|�@G
=@F��@F�y@Fff@E��@Ep�@D�/@C��@CS�@C"�@B�H@B��@B�\@A��@A��@AG�@@�`@@Ĝ@@�u@@Q�@@1'@@b@@  @?�w@?��@?K�@>�y@>�@>�@>��@>5?@>@=��@=�-@=��@=�h@=O�@<�/@<��@<z�@<�D@<Z@<9X@<�@<1@;�F@;o@:~�@9��@9��@9��@9�7@9hs@9X@9%@8��@81'@7��@7�P@7|�@7\)@7K�@7+@7
=@6�y@6ȴ@6�R@6��@6�+@6ff@6V@6V@6E�@6$�@6{@5��@5O�@4��@4�/@4�j@4Z@3��@3ƨ@3t�@3C�@2�@2��@2J@1�^@1G�@1%@0��@0�@01'@/�@/�@/K�@.�R@.�+@.$�@-��@-��@-@-@-�-@-O�@,�@,�@,1@+dZ@+@*�@*�@*��@*^5@*=q@*�@*J@)��@)��@)��@)7L@(�9@(Q�@(b@(b@(  @'�w@'K�@&��@&�@&�R@&��@&v�@&V@&5?@%�@%�h@%/@$�j@$�D@$Z@$1@#��@#@"�H@"��@"�!@"�!@"�\@"n�@"�@!�^@!X@!&�@ ��@ �u@ A�@��@��@;d@�y@��@V@�T@�h@�h@O�@�@�j@�@��@z�@(�@��@ƨ@��@��@��@t�@"�@@�@��@��@��@n�@M�@�@��@G�@�@%@��@��@�`@��@�@r�@bN@A�@b@�;@��@\)@�@��@�@�R@v�@$�@�@��@�h@`B@�@��@��@��@�D@z�@z�@z�@(�@��@S�@"�@@�@�@�H@��@��@��@��@�!@�!@��@��@~�@=q@��@�#@��@��@�7@X@7L@%@r�@1'@  @�@�@�@�;@��@�P@�@�y@�@ȴ@ȴ@ȴ@��@�+@ff@$�@{@�@�-@��@�h@?}@��@�/@�j@�@�@�D@I�@(�@1@��@�m@ƨ@ƨ@��@S�@33@
�@
�!@
^5@	�@	X@	G�@	&�@	%@��@��@�`@��@��@A�@b@�w@\)@K�@;d@+@��@$�@�@�T@��@��@?}@�@��@��@�D@�D@z�@I�@��@�m@��@t�@C�@@�H@�!@�\@~�@~�@~�@~�@~�@~�@n�@^5@^5@M�@-@�@��@�@�#@��@��@hs@7L@�@ ��@ �`@ �`@ Ĝ@ �9@ �@ �@ r�@ r�@ Q�@ A�@  �?��w?���?�|�?�;d?��?��?���?���?��?��R?�v�?�5??��?��-?�O�?�/?�/?�p�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A���A�ĜA֛�A։7A�=qAѓuAк^A�E�A���AϾwA�M�AΟ�A�t�A�ffA�XA�/Aͣ�A̗�A�l�AƩ�A���A�z�A��A��A��PA���A��A���A�\)A�l�A���A�\)A�VA�^5A�C�A��A�K�A�dZA� �A��TA�A�$�A�A�A�7LA��A�"�A��A�O�A�+A��#A��A��yA�A�33A�A�ffA�n�A���A�$�A��/A��A�I�A��wA��7A�ffA�ĜA�ĜA�A�\)A��A�M�A��A�p�A�`BA�1'A�l�A���A���A���A���A�{A�A�|�A���A��yA��A��A�K�A��DA���A�1'A��A�VA�A"�A}��A|M�Az��AwAv1'As?}Arz�Aq��Ap��Ao��Am�
AlI�AjȴAi��AhI�Ag��Af�jAe�mAdbNAct�Ab(�A`�HA`�DA`=qA^�A\M�A[;dA[VAZ�HAZM�AV{AT�AS�mASVARbAP��API�AP(�APbAO�hANbALM�AK|�AJĜAI�-AH�AHbAG`BAFn�AEXAD5?AC�ABE�AA��AAO�A@�A?�7A>�\A=��A=+A<�DA:�A9VA8�A8M�A7�PA733A6��A6��A6bNA5�;A5t�A5VA4{A2=qA1|�A1dZA1;dA0E�A/��A.��A.z�A-�7A,A�A+K�A*�uA*VA*bA)�A(  A&�/A&�DA&VA%S�A$  A#dZA"�\A!p�A -A�PA;dA�+A�PA�AffA�mA��AO�A�A(�At�A5?A�FA7LA�AjA��AK�A
=Av�AA�hA�A|�A"�A��A$�A��A��A�A�Ax�A
��A
A�A	��A	?}A�yA^5AhsA�RA�mA�Az�AAK�Av�At�A J@�E�@��@���@�1'@��m@���@�Ĝ@��@�h@�bN@��m@�@���@웦@�C�@�!@��@�D@�^5@�@��@�@�  @��y@݉7@��@��
@��H@��@ٙ�@أ�@�1'@ו�@�~�@�/@�S�@�@�bN@��@�%@˶F@ʰ!@�?}@�z�@��;@���@��T@���@Ĵ9@�
=@�=q@��@���@��u@� �@�dZ@�^5@��@��#@���@�&�@���@�j@�A�@��F@��H@��@�&�@�r�@�
=@�G�@���@�j@��F@�@��@�7L@�ȴ@�O�@�%@��/@���@� �@��@�ȴ@��\@��@�?}@�V@�Ĝ@��@��@��y@��H@��R@��@�hs@��@��@�r�@�j@�bN@�b@��y@��R@�@�&�@���@���@���@��-@��7@��@�G�@���@���@��@��@��+@�M�@��h@� �@��@�+@���@���@��-@�V@���@�Q�@�1'@��@��@�t�@�v�@���@�G�@��@��9@��u@�j@� �@���@��@���@�dZ@���@�~�@��@�X@��@��@�Ĝ@��9@��u@�z�@�b@�1@��m@��@�C�@�~�@��@��h@��@�p�@�X@��9@���@�ƨ@���@�l�@�dZ@�\)@�C�@���@���@�E�@��-@�O�@�&�@���@�I�@�(�@��
@�;d@��y@���@���@���@��\@��+@�~�@�ff@�$�@��-@���@�x�@�O�@��@��/@��9@��u@�9X@��@\)@~V@}�@}?}@}�@|��@|�j@|�D@|Z@|(�@{�
@{�@z�!@y�@y�^@yG�@y&�@y�@y&�@y7L@xbN@xQ�@x  @wl�@w+@w
=@v�y@v�@vȴ@v��@vV@v@u�@tZ@t�@s�
@so@r=q@q��@qG�@p��@p�u@pA�@pb@o��@o�@ol�@n��@n�+@nE�@m�@m��@m�h@mO�@l��@l�j@lj@k�m@kS�@k"�@ko@j�\@j�\@j~�@jn�@j=q@i��@i�^@i��@i�7@i�7@ihs@hr�@hA�@g�@g�P@gl�@gl�@g
=@fȴ@fv�@e��@e�-@e�@d�/@d9X@c��@c�
@c�F@c��@ct�@cC�@b��@bn�@bM�@bJ@ax�@a7L@a&�@a%@`b@_�P@^ȴ@^�+@^v�@^ff@^V@]p�@\��@\��@\j@\�@[�m@[��@Z�@Z��@Z=q@Y�^@Yx�@Yhs@Yhs@YX@X��@X�@X1'@W�w@W|�@W;d@V5?@U��@U�@T�/@T�@T9X@SC�@S@R�\@RM�@Q�#@Q��@Qhs@Q�@P�u@O�;@OK�@O+@N�R@N5?@M��@Mp�@M�@L�/@L��@K��@KS�@K"�@J��@J=q@I�#@I�^@I��@Ihs@I7L@H��@H �@G|�@G
=@F��@F�y@Fff@E��@Ep�@D�/@C��@CS�@C"�@B�H@B��@B�\@A��@A��@AG�@@�`@@Ĝ@@�u@@Q�@@1'@@b@@  @?�w@?��@?K�@>�y@>�@>�@>��@>5?@>@=��@=�-@=��@=�h@=O�@<�/@<��@<z�@<�D@<Z@<9X@<�@<1@;�F@;o@:~�@9��@9��@9��@9�7@9hs@9X@9%@8��@81'@7��@7�P@7|�@7\)@7K�@7+@7
=@6�y@6ȴ@6�R@6��@6�+@6ff@6V@6V@6E�@6$�@6{@5��@5O�@4��@4�/@4�j@4Z@3��@3ƨ@3t�@3C�@2�@2��@2J@1�^@1G�@1%@0��@0�@01'@/�@/�@/K�@.�R@.�+@.$�@-��@-��@-@-@-�-@-O�@,�@,�@,1@+dZ@+@*�@*�@*��@*^5@*=q@*�@*J@)��@)��@)��@)7L@(�9@(Q�@(b@(b@(  @'�w@'K�@&��@&�@&�R@&��@&v�@&V@&5?@%�@%�h@%/@$�j@$�D@$Z@$1@#��@#@"�H@"��@"�!@"�!@"�\@"n�@"�@!�^@!X@!&�@ ��@ �u@ A�@��@��@;d@�y@��@V@�T@�h@�h@O�@�@�j@�@��@z�@(�@��@ƨ@��@��@��@t�@"�@@�@��@��@��@n�@M�@�@��@G�@�@%@��@��@�`@��@�@r�@bN@A�@b@�;@��@\)@�@��@�@�R@v�@$�@�@��@�h@`B@�@��@��@��@�D@z�@z�@z�@(�@��@S�@"�@@�@�@�H@��@��@��@��@�!@�!@��@��@~�@=q@��@�#@��@��@�7@X@7L@%@r�@1'@  @�@�@�@�;@��@�P@�@�y@�@ȴ@ȴ@ȴ@��@�+@ff@$�@{@�@�-@��@�h@?}@��@�/@�j@�@�@�D@I�@(�@1@��@�m@ƨ@ƨ@��@S�@33@
�@
�!@
^5@	�@	X@	G�@	&�@	%@��@��@�`@��@��@A�@b@�w@\)@K�@;d@+@��@$�@�@�T@��@��@?}@�@��@��@�D@�D@z�@I�@��@�m@��@t�@C�@@�H@�!@�\@~�@~�@~�@~�@~�@~�@n�@^5@^5@M�@-@�@��@�@�#@��@��@hs@7L@�@ ��@ �`@ �`@ Ĝ@ �9@ �@ �@ r�@ r�@ Q�@ A�@  �?��w?���?�|�?�;d?��?��?���?���?��?��R?�v�?�5??��?��-?�O�?�/?�/?�p�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B<jB49B(�BB�B9XBB�BF�BF�BE�BC�BJ�BL�BK�BH�B@�B8RB/B<jB:^Bv�Bp�Bz�Bw�B�DB��B�7B�+B�1B��B��B�hB�VB�=B�B�Bx�Bm�BffBe`BXBC�B<jBA�B9XB/B7LB9XB5?B'�B�BhB
=B��B�5B�/B�B��B��B��BȴB��B�jB�FB��B�\By�By�Bx�Bk�BR�BS�BYBR�BC�B1'B&�BhB%B
��B
�fB
�;B
��B
ȴB
ŢB
B
�^B
�B
��B
�bB
~�B
x�B
w�B
l�B
^5B
K�B
A�B
!�B
"�B

=B
�B
\B
%B	��B	�B	�B	�5B	�/B	��B	�B	��B	ɺB	�}B	�qB	�^B	�-B	�RB	�'B	��B	�=B	�\B	�{B	�PB	|�B	\)B	T�B	o�B	ffB	_;B	VB	[#B	\)B	XB	M�B	;dB	+B	6FB	8RB	9XB	/B	49B	.B	)�B	!�B	�B	�B	�B	�B	�B	bB	VB	DB	B	%B��B�B�mB�B�B�yB�B�B�mB�`B�HB�/B�B��BÖB��B��BŢB�jB�LB�3B�-B��B��B��B��B��B��B��B�JB�PB�uB�hB�1B~�B�B� Bx�Bs�By�By�Bt�Bo�Br�Bq�Bq�Bs�Bp�Bk�Bl�BgmB`BBgmBffBgmBbNB^5BaHBbNB]/BZB^5B_;B\)BR�BA�BB�B@�BJ�BB�B:^B8RBA�B>wB@�B>wB>wB6FB2-B2-B33B1'B2-B1'B)�B$�B �B�B�B�B�B)�B+B#�B�B �B�B"�B'�B#�B�B�B�B�B�B�BVB1B�B�BoB�B�B�B�B�B�B!�B�B"�B"�B�B�B�B�B�B�B�B�B �B�B%�B&�B%�B(�B.B(�B%�B-B0!B33B49B6FB49B49B:^B:^B:^B:^B9XB<jB;dB8RB6FB7LB8RB<jB8RB:^BE�BH�BD�B>wBF�BE�B@�BA�BM�BN�BN�BJ�BJ�BR�BT�BR�BR�BYBZBW
B[#BaHBcTBaHB_;BbNBe`BffBiyBk�Bk�BiyBgmBn�Bp�Bn�Bq�Bo�Bl�Bt�B�B�B�B~�B�B�+B�PB�PB�VB�PB�PB��B��B��B��B��B��B��B�B�B�B�B�B�B�'B�jB��BƨBȴBȴBȴB��B��B��B��BɺB��B��B��B�#B�/B�5B�BB�BB�HB�BB�ZB�TB�NB�ZB�TB�yB�B�B�B�B�B�B	  B��B	  B	B	B	B	  B	B	B	B	DB	bB	bB	�B	�B	�B	�B	%�B	+B	-B	-B	/B	0!B	0!B	0!B	0!B	33B	;dB	<jB	=qB	>wB	@�B	B�B	C�B	C�B	F�B	F�B	I�B	O�B	VB	XB	XB	XB	YB	ZB	ZB	ZB	[#B	[#B	_;B	dZB	e`B	iyB	k�B	l�B	l�B	l�B	q�B	u�B	v�B	z�B	|�B	|�B	|�B	}�B	|�B	}�B	~�B	~�B	�B	�+B	�=B	�=B	�JB	�\B	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�!B	�3B	�3B	�9B	�9B	�?B	�FB	�LB	�RB	�RB	�LB	�?B	�dB	�jB	�wB	��B	��B	��B	B	B	ÖB	ƨB	ƨB	ƨB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�#B	�/B	�/B	�)B	�B	�/B	�;B	�HB	�HB	�NB	�NB	�HB	�ZB	�ZB	�`B	�mB	�yB	�yB	�yB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
+B
+B
+B
1B

=B

=B

=B

=B

=B
	7B
DB
JB
\B
VB
PB
PB
\B
\B
\B
oB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
!�B
 �B
 �B
�B
�B
 �B
 �B
$�B
%�B
%�B
$�B
$�B
$�B
$�B
%�B
&�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
(�B
)�B
)�B
+B
)�B
)�B
(�B
'�B
(�B
)�B
+B
+B
)�B
+B
,B
,B
-B
-B
.B
-B
/B
0!B
0!B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
49B
49B
5?B
6FB
6FB
6FB
6FB
5?B
5?B
6FB
5?B
6FB
8RB
:^B
9XB
9XB
8RB
:^B
:^B
:^B
:^B
:^B
;dB
:^B
9XB
;dB
<jB
=qB
=qB
<jB
<jB
=qB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
?}B
?}B
@�B
A�B
A�B
A�B
A�B
B�B
D�B
E�B
E�B
E�B
D�B
D�B
D�B
D�B
D�B
E�B
F�B
F�B
G�B
G�B
H�B
H�B
H�B
I�B
J�B
J�B
K�B
L�B
L�B
K�B
M�B
N�B
N�B
M�B
M�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
Q�B
Q�B
P�B
Q�B
Q�B
P�B
Q�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
VB
T�B
VB
VB
W
B
VB
W
B
W
B
XB
YB
YB
YB
YB
YB
YB
XB
W
B
YB
ZB
[#B
[#B
\)B
[#B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
[#B
[#B
[#B
\)B
]/B
]/B
]/B
]/B
]/B
]/B
\)B
^5B
_;B
`BB
`BB
`BB
`BB
_;B
^5B
^5B
`BB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
aHB
aHB
bNB
bNB
aHB
bNB
cTB
cTB
dZB
dZB
cTB
cTB
dZB
dZB
e`B
e`B
e`B
e`B
dZB
dZB
e`B
dZB
dZB
dZB
dZB
dZB
hsB
hsB
hsB
iyB
hsB
hsB
hsB
gmB
gmB
hsB
hsB
hsB
jB
jB
jB
iyB
gmB
k�B
l�B
l�B
k�B
k�B
l�B
l�B
m�B
n�B
n�B
n�B
m�B
m�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
q�B
r�B
q�B
r�B
q�B
q�B
q�B
s�B
s�B
t�B
s�B
s�B
t�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
w�B
x�B
x�B
w�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B<�B5B*�B	�B�B:�BC{BGEBGzBF�BD�BKBMBLBIlBB[B;�B5tBEmBC�B{�BvzB�4B~(B�BB�]B�"B��B��B�\B��B�,B�HB��B��B�B{0BpoBiBgRBZBE�B>wBB�B:�B0�B7�B9�B6FB)�BdB�B�B��B��B�VBרB��BөBΥBɆB��B�B��B��B�B}qB{dBy�Bm�BU�BT�BYeBS�BE�B3hB)BuBKB
��B
��B
�B
ԯB
ʌB
�?B
�B
�JB
��B
�qB
� B
��B
zxB
x�B
m�B
_�B
M�B
C�B
%FB
$�B
�B
�B
bB
�B
 �B	��B	�qB	�B	�jB	бB	��B	� B	��B	�UB	��B	�B	��B	��B	��B	��B	�6B	��B	��B	�B	~�B	`�B	WsB	o�B	g�B	`�B	WsB	[�B	\xB	XyB	N�B	=qB	-CB	7fB	9rB	:�B	0�B	4�B	/B	+QB	#TB	;B	B	�B	qB	QB	�B	BB	~B	�B	�B	 OB��B�B�B��B�eB�B��B��B��B��B��B��B�bBŢBB��B�?B��B�8B�9B��B�kB�OB��B��B�FB�bB��B�VB��B��B� B��B��B��B�UBzxBuZBz�Bz�Bu�Bp�BshBr�BraBt9Bq[Bl�Bm]Bh�Ba�Bh$BgBg�BcTB_VBa�Bb�B^B[	B^�B_pB\�BS�BC�BD�BBBK�BC�B<B9�BBuB?}BA;B?cB?.B7fB3�B3MB4TB2GB3B1�B+QB&LB"hB�BB~B/B*B+�B$�BB!�BB#�B(XB$�B �B�B�BjBBmB�B	�BOBB�B�B�BVB�BjB \B"NB vB#TB#�B�B�B�B�B�B+B�B�B!�B �B&�B'�B&�B)�B.cB)�B'B-�B0�B3�B4�B6�B4�B5B:�B:�B:�B:�B9�B<�B;�B8�B6�B8B9>B="B9rB;�BFBIBEmB?�BGBF�BB'BB�BN"BO(BOBKxBKxBS@BUMBSuBS�BYBZ�BW�B[�Ba|Bc�Ba�B_�Bb�Be�Bf�Bi�Bk�Bk�Bi�BhXBo Bq[Bo5BrBp�Bm�ButB�;B�[B�UB�B��B��B��B��B��B�B�VB��B��B�B�!B��B�hB�LB�6B�/B�cB�]B��B��B��B��B��B��B��B�B�B�B��B�B�0B�=B�(BуBՁB�WB�dB�jB�vB��B�B��B�B�B��B�B��B��B� B��B��B��B�AB�MB	 B�.B	 4B	'B	AB	;B	 iB	oB	mB	�B	�B	�B	�B	�B	�B	�B	/B	&B	+B	-)B	-)B	/OB	0;B	0;B	0oB	0oB	3�B	;�B	<�B	=�B	>�B	@�B	B�B	C�B	C�B	F�B	GB	J=B	P.B	VB	X+B	X+B	X+B	Y1B	ZQB	Z7B	ZQB	[WB	[�B	_�B	d�B	e�B	i�B	k�B	l�B	l�B	l�B	q�B	u�B	v�B	z�B	}"B	}B	}B	~(B	}B	~BB	.B	}B	�aB	�_B	�rB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	�B	�$B	�*B	�=B	�OB	�;B	�UB	�MB	�MB	�TB	�TB	�tB	�`B	�fB	�RB	�lB	�fB	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�B	�B	��B	�B	�"B	��B	� B	� B	�4B	�&B	�B	�MB	�uB	�9B	�_B	�=B	�IB	�IB	�]B	چB	�dB	�pB	�bB	�bB	�hB	�B	�B	�tB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	��B	��B	��B	��B	��B	�B	��B	��B	�	B	�	B	��B	��B	�*B	�$B	�JB	�"B	�B	�.B	�.B
 4B
;B
'B
-B
GB
[B
9B
EB
_B
_B
fB

XB

XB

XB

XB

rB
	�B
xB
~B
\B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
!�B
 �B
 �B
�B
!B
 �B
!-B
$�B
%�B
%�B
$�B
%B
$�B
%B
&B
'B
($B
(�B
)*B
)*B
)B
)B
)B
*B
)�B
*B
)B
*B
*B
+B
*B
*0B
)B
(>B
)*B
*0B
+6B
+B
*0B
+B
,"B
,"B
-)B
-CB
.IB
-CB
/OB
0UB
0UB
1AB
1AB
1[B
2aB
2GB
2aB
2aB
4TB
4TB
5tB
6FB
6`B
6FB
6zB
5tB
5�B
6`B
5�B
6zB
8�B
:^B
9XB
9�B
8�B
:xB
:�B
:xB
:xB
:xB
;B
:�B
9�B
;B
<�B
=qB
=�B
<�B
<�B
=�B
>�B
>�B
?�B
?�B
?�B
?�B
?�B
?�B
?�B
@�B
A�B
A�B
A�B
A�B
B�B
D�B
E�B
E�B
E�B
D�B
D�B
D�B
D�B
D�B
E�B
F�B
F�B
G�B
G�B
H�B
IB
H�B
I�B
J�B
KB
K�B
L�B
L�B
K�B
M�B
N�B
N�B
NB
M�B
N�B
N�B
PB
O�B
PB
O�B
O�B
QB
Q B
Q B
Q�B
RB
QB
R B
RB
QB
RB
TB
S�B
S�B
S�B
S�B
T,B
TB
T�B
UB
TB
T,B
T,B
T,B
TB
U2B
UB
U2B
V9B
UB
VB
V9B
W?B
VB
W$B
W$B
XEB
YB
YKB
Y1B
YB
Y1B
Y1B
XEB
W?B
Y1B
Z7B
[#B
[#B
\CB
[=B
\)B
\CB
\)B
\)B
\CB
\)B
\)B
\CB
[WB
[WB
[WB
\CB
]/B
]dB
]IB
]IB
]IB
]IB
\]B
^OB
_pB
`\B
`BB
`BB
`\B
_pB
^OB
^jB
`\B
aHB
abB
aHB
aHB
abB
abB
a|B
a|B
bNB
abB
abB
bhB
bhB
abB
bhB
c�B
cnB
dZB
dtB
c�B
cnB
d�B
d�B
e`B
ezB
ezB
e`B
dtB
dtB
ezB
dtB
dtB
d�B
d�B
d�B
h�B
h�B
h�B
i�B
h�B
hsB
h�B
g�B
g�B
h�B
h�B
h�B
j�B
jB
j�B
i�B
g�B
k�B
l�B
l�B
k�B
k�B
l�B
l�B
m�B
n�B
n�B
n�B
m�B
m�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
q�B
r�B
q�B
r�B
q�B
q�B
q�B
s�B
s�B
t�B
s�B
s�B
t�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
w�B
x�B
x�B
w�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.07(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201808250034002018082500340020180825003400201808250200182018082502001820180825020018201808260025422018082600254220180826002542  JA  ARFMdecpA19c                                                                20180821093512  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180821003514  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180821003517  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180821003517  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180821003518  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180821003518  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180821003518  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180821003518  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180821003519  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180821003519                      G�O�G�O�G�O�                JA  ARUP                                                                        20180821005558                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180821153313  CV  JULD            G�O�G�O�F�ٱ                JM  ARCAJMQC2.0                                                                 20180824153400  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180824153400  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180824170018  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180825152542  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115131518                      G�O�G�O�G�O�                