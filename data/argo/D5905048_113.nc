CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-04-28T00:35:23Z creation;2017-04-28T00:35:26Z conversion to V3.1;2019-12-19T08:09:06Z update;     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20170428003523  20200116211516  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               qA   JA  I2_0577_113                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�6!/h�1   @�6�-� @2���҉�d�����1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A���A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#�fD$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dc��Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dσ3D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�<�D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�(�@�@�A�HA>�HA^�HA~�HA�p�A�p�A�p�A�p�A�=qA�p�A�p�A�p�B�RB�RB�RB�RB'�RB/�RB7�RB?�RBG�RBO�RBW�RB_�RBg�RBo�RBw�RB�RB��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��C��C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
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
D {�D ��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D	{�D	��D
{�D
��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D�D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D {�D ��D!{�D!��D"{�D"��D#��D#��D${�D$��D%{�D%��D&{�D&��D'{�D'��D({�D(��D){�D)��D*{�D*��D+{�D+��D,{�D,��D-{�D-��D.{�D.��D/{�D/��D0{�D0��D1{�D1��D2{�D2��D3{�D3��D4{�D4��D5{�D5��D6{�D6��D7{�D7��D8{�D8��D9{�D9��D:{�D:��D;{�D;��D<{�D<��D={�D=��D>{�D>��D?{�D?��D@{�D@��DA{�DA��DB{�DB��DC{�DC��DD{�DD��DE{�DE��DF{�DF��DG{�DG��DH{�DH��DI{�DI��DJ{�DJ��DK{�DK��DL{�DL��DM{�DM��DN{�DN��DO{�DO��DP{�DP��DQ{�DQ��DR{�DR��DS{�DS��DT{�DT��DU{�DU��DV{�DV��DW{�DW��DX{�DX��DY{�DY��DZ{�DZ��D[{�D[��D\{�D\��D]{�D]��D^{�D^��D_{�D_��D`{�D`��Da{�Da��Db{�Db��Dc{�Dc�Dd{�Dd��De{�De��Df{�Df��Dg{�Dg��Dh{�Dh��Di{�Di��Dj{�Dj��Dk{�Dk��Dl{�Dl��Dm{�Dm��Dn{�Dn��Do{�Do��Dp{�Dp��Dq{�Dq��Dr{�Dr��Ds{�Ds��Dt{�Dt��Du{�Du��Dv{�Dv��Dw{�Dw��Dx{�Dx��Dy{�Dy��Dz{�Dz��D{{�D{��D|{�D|��D}{�D}��D~{�D~��D{�D��D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D½�D���D�=�D�}�Dý�D���D�=�D�}�DĽ�D���D�=�D�}�DŽ�D���D�=�D�}�Dƽ�D���D�=�D�}�Dǽ�D���D�=�D�}�DȽ�D���D�=�D�}�Dɽ�D���D�=�D�}�Dʽ�D���D�=�D�}�D˽�D���D�=�D�}�D̽�D���D�=�D�}�Dͽ�D���D�=�D�}�Dν�D���D�=�Dπ�DϽ�D���D�=�D�}�Dн�D���D�=�D�}�Dѽ�D���D�=�D�}�Dҽ�D���D�=�D�}�Dӽ�D���D�=�D�}�DԽ�D���D�=�D�}�Dս�D���D�=�D�}�Dֽ�D���D�=�D�}�D׽�D���D�=�D�}�Dؽ�D���D�=�D�}�Dٽ�D���D�=�D�}�Dڽ�D���D�=�D�}�D۽�D���D�=�D�}�Dܽ�D���D�=�D�}�Dݽ�D���D�=�D�}�D޽�D���D�=�D�}�D߽�D���D�:�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�z�D躏D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D���D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�v�A�t�A�t�A�t�A�v�A�v�A�v�A�t�A�t�A�r�A�r�A�r�A�r�A�r�A�p�A�n�A�n�A�n�A�l�A�hsA��AБhA�(�A��Aϛ�A΍PA�K�A��Ȁ\A���A�5?A��AʮAʲ-A��mA��A���A�S�A�1AɑhAɼjAɾwA�n�AȸRAȓuAȬA��/A�n�A�jA�dZA�hsA�\)A�Q�A�;dA��A��HA���Aǩ�A��AƧ�AŋDA���A�z�A���A�9XA�oA�7LA�&�A���A�oA�;dA�~�A��A���A�v�A�ĜA��9A�ffA�33A���A��A��DA�^5A�ZA���A�"�A��TA��A���A�A��A�bNA�|�A�9XA��A�l�A�
=A��#A�1A� �A��`A�ZA�33A���A���A��hA���A�^5A��^A�+A��HA���A�n�A��HA��A��+A�ffA���A�r�A��A���A��mA���A�
=A���A�t�A���A�^5A��+A}��Az�`Ay�
Ax�Ax��Aw�mAv��Au�Atn�Ar�Ap��An �Ai��AfI�Ae�TAe�-Ad��Aa�FA_%A^�A\��AZ�HAW�FAVE�AU��ATI�AR5?AQK�APn�AO��AN�RAMl�AL��AK�wAK�AI"�AHJAF��AB��AA?}A@Q�A?�A?��A>��A=�#A<��A;�mA:ZA7�7A5oA2��A21A/VA-O�A)��A(v�A'XA&�A&$�A$~�A#�7A"n�A!��A!x�A�^A�A&�Ap�AhsA�`A9XA��A/A��Ar�A\)A��A=qA�;AO�A�HA�mA�A9XA
��A	`BA��A�A&�AȴA;dAz�A�wA�`A�A�hA �\A J@�
=@�`B@��@��@���@�7L@���@��@���@�$�@��7@���@�E�@�  @��H@�D@�ȴ@���@�x�@��m@�\)@��H@�R@�@�J@��@�\)@�`B@�V@��@���@��@㕁@�;d@��H@�$�@�j@߅@ݺ^@��@�(�@�ff@�hs@��@�Q�@��H@Ձ@��/@�A�@�"�@ҟ�@�J@�G�@Л�@�1@�hs@���@�+@�n�@ɑh@��@�b@�{@��@��@Ĵ9@��@���@�E�@�{@���@�/@�/@��@�r�@�l�@�v�@�-@��@��-@��7@�O�@���@��@�~�@�v�@�ff@�v�@��T@��/@��;@�E�@�%@��@��j@��u@�1'@�l�@�+@��@���@�ff@�E�@�=q@�@���@�/@�j@��
@�t�@��y@���@���@�-@���@��7@���@�Z@� �@�b@�j@�bN@��
@�\)@��\@�v�@�V@�-@�$�@���@��h@�x�@�p�@�hs@�X@�?}@���@���@��F@��@��@��^@��7@��h@��^@���@��^@���@��#@���@�?}@�%@�%@���@��9@�1@�j@�r�@�Z@���@�C�@�S�@�C�@��@�^5@�@��T@��^@�x�@�&�@��@�A�@�Ĝ@��;@��@�
=@�"�@���@��!@�=q@�@�x�@�X@��`@��u@�j@��@���@��`@��9@���@���@�Q�@�A�@�1'@� �@�b@��m@�\)@�33@�33@��@�-@�@��@���@��@��@��D@�Z@�Ĝ@���@���@���@�r�@��;@�1@��P@��@��H@���@�V@�J@�J@��@���@��h@��7@��@���@��9@�bN@��@�ƨ@��F@�t�@��H@�~�@�$�@��T@��h@�O�@��@���@�z�@��D@��D@��@��u@��D@�Z@�b@��
@��@��@�l�@�dZ@�K�@��@���@�n�@�=q@��@��@���@��h@�7L@��@��@��@�Q�@�l�@��@��H@��@���@��+@�{@���@��h@��@�x�@�x�@�p�@�hs@�&�@��@��/@���@��@�I�@� �@��@���@���@�;d@��@���@��+@�=q@��@�@�%@��/@��9@��@��D@�bN@��P@�K�@��@���@���@�ff@�@��h@�p�@�7L@��@���@���@�Ĝ@��9@�z�@��@�  @\)@~ȴ@~��@~{@}�h@|�@|��@|1@{��@{t�@{33@z�!@y��@x��@x �@w��@w�@w��@wK�@v��@v$�@u�T@u�-@u`B@t��@t�@t9X@s�m@s�m@s�
@sC�@r��@q�#@q�@p�`@p��@pQ�@p1'@p �@o�@o|�@o;d@o+@nȴ@n�+@nv�@nE�@m�T@m��@m?}@l��@lj@k��@kƨ@k��@kt�@kdZ@kt�@kdZ@j�H@j��@jM�@i�^@i��@i�7@ihs@h��@hr�@h1'@g��@gl�@gK�@g
=@f$�@e��@e?}@d�@d�D@dz�@dI�@c�m@c"�@c@b�H@b��@b��@b��@b��@b��@a�@aX@`�`@`A�@_�@_l�@^ȴ@^ȴ@^��@^��@^��@^@\�@\(�@[��@[��@["�@Z�@Z^5@ZJ@Y�^@YG�@Y�@X��@XA�@W��@W��@WK�@Vȴ@VE�@V@U`B@T�@TZ@T(�@S��@S�
@S��@R�@R�!@Q��@Q�7@Q7L@P�`@PĜ@P�u@P�@PbN@P �@O��@O\)@N�@N��@NV@N{@M�h@M�@L�@L�@K��@KS�@K@J�\@J^5@J=q@JJ@I�@I�^@Ix�@IX@I&�@H��@H �@G�@G�w@G�P@GK�@F��@F�R@Fv�@F5?@E@EO�@EV@D�@Dz�@Dj@D9X@D(�@Cƨ@CdZ@C33@Co@B�@B�H@B�@AG�@A7L@A&�@@�u@@ �@?��@?��@?|�@?l�@?K�@?+@?
=@>��@>�y@>�+@=�@=@=�h@<��@;��@;�F@;S�@;"�@;"�@;o@:�H@:�!@:��@:~�@:^5@:J@9�@9��@9hs@9X@9G�@9&�@8��@8�@8  @7\)@6��@6�+@6v�@6ff@6V@65?@6{@5@5`B@4�@4��@3�m@3�@3C�@3o@2�@2��@2^5@2J@1��@1hs@0��@0��@0Ĝ@0�9@0A�@/�;@/\)@.��@.�R@.{@-�@-��@-p�@-O�@,��@,�@+�F@+��@+C�@*�H@*��@*n�@*=q@*J@)�#@)��@)��@)��@)��@)x�@)&�@(��@(��@(��@(A�@( �@(b@'�@'�w@'��@'l�@'+@&�y@&��@&ff@&5?@&$�@&@%�-@%`B@%�@$��@$�@$I�@#��@#�@#"�@"�\@"=q@"=q@"J@!�#@!�^@!�7@!X@!&�@ ��@ �u@ r�@   @�@|�@\)@�@��@5?@{@@�@��@��@@`B@��@�@�D@I�@9X@��@ƨ@��@t�@o@�!@��@�\@�\@M�@J@�^@hs@&�@�`@��@bN@1'@�;@��@�P@l�@l�@�@��@�@ȴ@�R@v�@{@�@@�@p�@`B@?}@V@��@��@�D@j@(�@1@��@�m@�
@�
@ƨ@�F@��@33@��@��@��@�\@M�@J@�@�#@�^@�7@x�@hs@X@%@Ĝ@�9@��@�u@�@A�@ �@b@�;@|�@�@
=@�y@�@�@ȴ@�R@��@V@{@@�@�T@�T@��@�h@`B@O�@��@z�@Z@9X@9X@(�@�@ƨ@t�@S�@33@"�@"�@"�@o@
�@
��@
�\111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�v�A�t�A�t�A�t�A�v�A�v�A�v�A�t�A�t�A�r�A�r�A�r�A�r�A�r�A�p�A�n�A�n�A�n�A�l�A�hsA��AБhA�(�A��Aϛ�A΍PA�K�A��Ȁ\A���A�5?A��AʮAʲ-A��mA��A���A�S�A�1AɑhAɼjAɾwA�n�AȸRAȓuAȬA��/A�n�A�jA�dZA�hsA�\)A�Q�A�;dA��A��HA���Aǩ�A��AƧ�AŋDA���A�z�A���A�9XA�oA�7LA�&�A���A�oA�;dA�~�A��A���A�v�A�ĜA��9A�ffA�33A���A��A��DA�^5A�ZA���A�"�A��TA��A���A�A��A�bNA�|�A�9XA��A�l�A�
=A��#A�1A� �A��`A�ZA�33A���A���A��hA���A�^5A��^A�+A��HA���A�n�A��HA��A��+A�ffA���A�r�A��A���A��mA���A�
=A���A�t�A���A�^5A��+A}��Az�`Ay�
Ax�Ax��Aw�mAv��Au�Atn�Ar�Ap��An �Ai��AfI�Ae�TAe�-Ad��Aa�FA_%A^�A\��AZ�HAW�FAVE�AU��ATI�AR5?AQK�APn�AO��AN�RAMl�AL��AK�wAK�AI"�AHJAF��AB��AA?}A@Q�A?�A?��A>��A=�#A<��A;�mA:ZA7�7A5oA2��A21A/VA-O�A)��A(v�A'XA&�A&$�A$~�A#�7A"n�A!��A!x�A�^A�A&�Ap�AhsA�`A9XA��A/A��Ar�A\)A��A=qA�;AO�A�HA�mA�A9XA
��A	`BA��A�A&�AȴA;dAz�A�wA�`A�A�hA �\A J@�
=@�`B@��@��@���@�7L@���@��@���@�$�@��7@���@�E�@�  @��H@�D@�ȴ@���@�x�@��m@�\)@��H@�R@�@�J@��@�\)@�`B@�V@��@���@��@㕁@�;d@��H@�$�@�j@߅@ݺ^@��@�(�@�ff@�hs@��@�Q�@��H@Ձ@��/@�A�@�"�@ҟ�@�J@�G�@Л�@�1@�hs@���@�+@�n�@ɑh@��@�b@�{@��@��@Ĵ9@��@���@�E�@�{@���@�/@�/@��@�r�@�l�@�v�@�-@��@��-@��7@�O�@���@��@�~�@�v�@�ff@�v�@��T@��/@��;@�E�@�%@��@��j@��u@�1'@�l�@�+@��@���@�ff@�E�@�=q@�@���@�/@�j@��
@�t�@��y@���@���@�-@���@��7@���@�Z@� �@�b@�j@�bN@��
@�\)@��\@�v�@�V@�-@�$�@���@��h@�x�@�p�@�hs@�X@�?}@���@���@��F@��@��@��^@��7@��h@��^@���@��^@���@��#@���@�?}@�%@�%@���@��9@�1@�j@�r�@�Z@���@�C�@�S�@�C�@��@�^5@�@��T@��^@�x�@�&�@��@�A�@�Ĝ@��;@��@�
=@�"�@���@��!@�=q@�@�x�@�X@��`@��u@�j@��@���@��`@��9@���@���@�Q�@�A�@�1'@� �@�b@��m@�\)@�33@�33@��@�-@�@��@���@��@��@��D@�Z@�Ĝ@���@���@���@�r�@��;@�1@��P@��@��H@���@�V@�J@�J@��@���@��h@��7@��@���@��9@�bN@��@�ƨ@��F@�t�@��H@�~�@�$�@��T@��h@�O�@��@���@�z�@��D@��D@��@��u@��D@�Z@�b@��
@��@��@�l�@�dZ@�K�@��@���@�n�@�=q@��@��@���@��h@�7L@��@��@��@�Q�@�l�@��@��H@��@���@��+@�{@���@��h@��@�x�@�x�@�p�@�hs@�&�@��@��/@���@��@�I�@� �@��@���@���@�;d@��@���@��+@�=q@��@�@�%@��/@��9@��@��D@�bN@��P@�K�@��@���@���@�ff@�@��h@�p�@�7L@��@���@���@�Ĝ@��9@�z�@��@�  @\)@~ȴ@~��@~{@}�h@|�@|��@|1@{��@{t�@{33@z�!@y��@x��@x �@w��@w�@w��@wK�@v��@v$�@u�T@u�-@u`B@t��@t�@t9X@s�m@s�m@s�
@sC�@r��@q�#@q�@p�`@p��@pQ�@p1'@p �@o�@o|�@o;d@o+@nȴ@n�+@nv�@nE�@m�T@m��@m?}@l��@lj@k��@kƨ@k��@kt�@kdZ@kt�@kdZ@j�H@j��@jM�@i�^@i��@i�7@ihs@h��@hr�@h1'@g��@gl�@gK�@g
=@f$�@e��@e?}@d�@d�D@dz�@dI�@c�m@c"�@c@b�H@b��@b��@b��@b��@b��@a�@aX@`�`@`A�@_�@_l�@^ȴ@^ȴ@^��@^��@^��@^@\�@\(�@[��@[��@["�@Z�@Z^5@ZJ@Y�^@YG�@Y�@X��@XA�@W��@W��@WK�@Vȴ@VE�@V@U`B@T�@TZ@T(�@S��@S�
@S��@R�@R�!@Q��@Q�7@Q7L@P�`@PĜ@P�u@P�@PbN@P �@O��@O\)@N�@N��@NV@N{@M�h@M�@L�@L�@K��@KS�@K@J�\@J^5@J=q@JJ@I�@I�^@Ix�@IX@I&�@H��@H �@G�@G�w@G�P@GK�@F��@F�R@Fv�@F5?@E@EO�@EV@D�@Dz�@Dj@D9X@D(�@Cƨ@CdZ@C33@Co@B�@B�H@B�@AG�@A7L@A&�@@�u@@ �@?��@?��@?|�@?l�@?K�@?+@?
=@>��@>�y@>�+@=�@=@=�h@<��@;��@;�F@;S�@;"�@;"�@;o@:�H@:�!@:��@:~�@:^5@:J@9�@9��@9hs@9X@9G�@9&�@8��@8�@8  @7\)@6��@6�+@6v�@6ff@6V@65?@6{@5@5`B@4�@4��@3�m@3�@3C�@3o@2�@2��@2^5@2J@1��@1hs@0��@0��@0Ĝ@0�9@0A�@/�;@/\)@.��@.�R@.{@-�@-��@-p�@-O�@,��@,�@+�F@+��@+C�@*�H@*��@*n�@*=q@*J@)�#@)��@)��@)��@)��@)x�@)&�@(��@(��@(��@(A�@( �@(b@'�@'�w@'��@'l�@'+@&�y@&��@&ff@&5?@&$�@&@%�-@%`B@%�@$��@$�@$I�@#��@#�@#"�@"�\@"=q@"=q@"J@!�#@!�^@!�7@!X@!&�@ ��@ �u@ r�@   @�@|�@\)@�@��@5?@{@@�@��@��@@`B@��@�@�D@I�@9X@��@ƨ@��@t�@o@�!@��@�\@�\@M�@J@�^@hs@&�@�`@��@bN@1'@�;@��@�P@l�@l�@�@��@�@ȴ@�R@v�@{@�@@�@p�@`B@?}@V@��@��@�D@j@(�@1@��@�m@�
@�
@ƨ@�F@��@33@��@��@��@�\@M�@J@�@�#@�^@�7@x�@hs@X@%@Ĝ@�9@��@�u@�@A�@ �@b@�;@|�@�@
=@�y@�@�@ȴ@�R@��@V@{@@�@�T@�T@��@�h@`B@O�@��@z�@Z@9X@9X@(�@�@ƨ@t�@S�@33@"�@"�@"�@o@
�@
��@
�\111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
hB
bB
bB
hB
hB
hB
hB
hB
bB
bB
hB
bB
bB
bB
bB
bB
bB
bB
bB
bB
{B
 �B
"�B
 �B
'�B
2-B
N�B
m�B
�%B
�JB
�B
{�B
{�B
�\B
�LB
��B
��B
��B
��B
��B
�B
��B1BoB�B$�BM�Bz�B�hB�uB�B�^B��B��B�B�NB�`B�fB��B��BBDBuB$�B7LBE�B;dB2-B!�B&�B-B,B,B2-B=qB;dB:^B8RB8RB6FB2-B.B0!B0!B7LB8RB6FB33B-B33B5?B0!B�B+B��BB�BL�B;dB/B�B%�B'�B"�BhB\BB�#B�FB�B�B��B��B�oB�1Bn�BZBC�B�BuBB
��B
�B
�yB
�B
ƨB
�LB
��B
��B
�+B
o�B
iyB
aHB
^5B
ZB
Q�B
J�B
@�B
5?B
$�B
\B	�B	��B	��B	ɺB	B	�?B	��B	�hB	�7B	{�B	k�B	aHB	\)B	VB	I�B	A�B	;dB	7LB	2-B	,B	)�B	#�B	#�B	�B	uB	PB��B�B�mB�ZB�NB�;B�#B��B��B��B��BÖB�^B�-B�B��B��B�\B�DB�1B�%B�B~�B}�B{�Bz�Bt�BiyBcTB`BB[#BZBYBXBW
BT�BT�BS�BS�BVBW
BXBZB]/B_;B_;B[#BS�BW
BZB[#BZB\)B[#B_;B`BBbNBaHBffBgmBgmBjBk�Bm�Bv�Bw�By�B}�B�B�B�B�1B�Bx�By�B}�B� B�B� B�B�1B�7B�PB�bB�hB��B��B�XB�wBBÖBĜBǮBȴBɺB��B��B��B��B�B�/B�ZB�fB�fB�yB�B�B�B�B�B��B��B��B��B��B	B	  B	  B��B	B	B	B	1B		7B	
=B	DB	PB	\B	hB	hB	oB	{B	{B	{B	{B	�B	�B	�B	�B	�B	�B	�B	!�B	"�B	%�B	'�B	(�B	)�B	-B	/B	2-B	6FB	7LB	9XB	;dB	=qB	@�B	E�B	G�B	H�B	I�B	L�B	M�B	M�B	N�B	O�B	R�B	W
B	YB	\)B	\)B	_;B	bNB	e`B	ffB	gmB	gmB	ffB	hsB	o�B	t�B	u�B	t�B	t�B	u�B	w�B	w�B	y�B	|�B	� B	�B	�B	�B	�B	�B	�B	�B	�1B	�7B	�7B	�+B	�7B	�=B	�PB	�bB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�B	�!B	�-B	�LB	�FB	�3B	�?B	�RB	�RB	�RB	�RB	�^B	�^B	�dB	�dB	�dB	�jB	��B	B	ŢB	ƨB	ɺB	��B	��B	��B	��B	�
B	�B	�B	�B	�#B	�/B	�/B	�)B	�)B	�;B	�ZB	�sB	�`B	�ZB	�fB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
1B
	7B
	7B

=B

=B
DB
DB
DB
DB
DB
DB
DB
JB
JB
JB
JB
JB
JB
JB
JB
JB
DB
DB
DB
PB
VB
VB
VB
\B
\B
oB
oB
uB
uB
uB
{B
{B
�B
�B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
 �B
 �B
!�B
!�B
!�B
!�B
!�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
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
)�B
+B
+B
+B
,B
-B
/B
/B
/B
0!B
1'B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
2-B
2-B
33B
49B
49B
49B
49B
49B
49B
5?B
5?B
6FB
6FB
6FB
6FB
6FB
6FB
6FB
6FB
6FB
7LB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
8RB
7LB
8RB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
:^B
;dB
;dB
;dB
;dB
;dB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
?}B
?}B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
O�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
VB
VB
W
B
W
B
XB
XB
XB
XB
XB
XB
YB
YB
YB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
\)B
\)B
\)B
]/B
\)B
]/B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
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
ffB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
jB
iyB
iyB
iyB
jB
jB
jB
jB
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
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
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
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
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
x�B
w�B
w�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{�B
{�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
hB
bB
}B
hB
hB
hB
hB
�B
bB
bB
hB
bB
bB
bB
bB
bB
}B
�B
�B
�B
�B
!�B
#�B
!�B
*0B
4nB
O�B
n�B
�zB
�PB
��B
|6B
|B
�BB
�fB
�B
͹B
ΥB
ѷB
��B
�B
��B	RB�B~B%,BN�Bz�B�hB��B� B�xB��B�"B֡B�B��B�B�B�.B�BdBB&�B9�BG�B>B5�B%,B)DB0oB.cB.cB5B@�B=�B;�B;0B;�B9rB4B1'B3�B1�B88B9	B7LB5ZB/�B5tB9$B3�B�B
=B��BgBOBP.B=�B1vBB&�B)�B$�B�B:B�B�jB��B�B�=B�B�dB��B��Bq�B^�BG�B �B�B�B
��B
�vB
�WB
�dB
�7B
�DB
��B
�]B
�	B
qB
j�B
a�B
_;B
[�B
S&B
L�B
B�B
8RB
(�B
,B	��B	ѝB	�~B	˒B	�?B	�B	�VB	��B	��B	HB	m]B	b�B	^B	X+B	KB	B�B	<�B	8�B	3�B	-CB	+QB	%`B	&B	]B	�B	hB��B��B�$B�B�nB��B��B֡B�{B׍B��B�?B�jB��B��B��B�QB��B�dB�lB�KB��B�iBB|�B}�Bw�Bl�Be�Bb�B\B[=BZBY1BYBV�BVSBT�BT�BV�BW�BX�B[�B^�B`�Ba-B\�BUBW�B[WB\)B\)B]IB\]B`vBa�Bc:Bb�Bg8BhXBh�Bk�Bk�Bn�Bw2Bx8Bz�B~�B�uB��B��B�XB�{BzB{BB��B��B��B��B��B��B��B�bB�NB��B�B��B��B�B��B��B��B�7B�rB��B��B�9BյB��B�OB�B�B�B�B�qB�B�/B�cB�AB��B��B��B��B��B	�B	 �B	 �B��B	�B	�B	SB	�B		�B	
�B	�B	B	�B	�B	�B	�B	�B	�B	B	2B	+B	�B	�B	�B	�B	�B	5B	#B	#:B	%�B	(
B	)DB	*�B	-�B	/�B	33B	7B	7�B	9rB	;�B	=�B	AB	E�B	G�B	IB	I�B	L�B	M�B	N<B	OBB	PbB	S�B	WsB	YeB	\xB	\xB	_pB	b�B	e�B	f�B	g�B	g�B	f�B	h�B	o�B	t�B	vFB	u?B	u?B	u�B	xB	xB	y�B	}"B	�4B	�;B	�'B	�GB	�3B	�MB	�gB	��B	��B	��B	��B	�zB	�RB	�=B	�PB	�bB	��B	�uB	��B	��B	�B	��B	��B	��B	�B	�B	��B	��B	�>B	�_B	�$B	�B	�B	�QB	�]B	�wB	�OB	�iB	�iB	��B	�iB	�oB	�GB	�B	��B	�MB	�ZB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�jB	��B	B	żB	��B	��B	��B	��B	� B	�B	�$B	�KB	�B	�eB	�=B	�~B	ݲB	�]B	�)B	�pB	�tB	��B	�B	�B	�2B	�B	�B	��B	��B	��B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	�B	��B	�VB	�BB	�.B
 4B
 OB
 4B
 B
UB
�B
[B
aB
aB
GB
aB
gB
�B
-B
B
?B
%B
?B
KB
	�B
	�B

rB

XB
^B
^B
^B
xB
�B
�B
xB
~B
dB
~B
dB
~B
~B
dB
~B
~B
�B
�B
�B
jB
�B
pB
�B
�B
�B
oB
�B
uB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
B
�B
�B
�B
�B
�B
�B
 'B
!-B
!B
!�B
!�B
!�B
!�B
!�B
# B
"�B
#B
#�B
$B
$B
$B
$�B
$�B
%B
%B
%,B
&LB
&2B
'B
'B
'B
'B
'B
'B
'8B
($B
(
B
($B
)B
)*B
)B
)*B
)*B
)DB
*KB
*0B
*KB
*B
+6B
+6B
+B
,"B
-)B
/iB
/OB
/iB
0UB
1[B
1[B
1AB
1[B
2aB
2aB
2aB
2GB
2GB
2|B
3�B
4�B
4nB
4�B
4TB
4nB
4TB
5�B
5tB
6`B
6`B
6`B
6FB
6`B
6`B
6zB
6�B
6�B
7�B
7�B
7�B
7fB
8�B
8RB
8lB
8RB
8�B
7�B
8�B
9�B
9rB
9�B
:�B
:xB
:�B
:�B
:�B
;�B
;B
;�B
;�B
;�B
<�B
<�B
<�B
<�B
<�B
<�B
<�B
<�B
<�B
=�B
=�B
=�B
=�B
>�B
>�B
>�B
>�B
?�B
?�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
H�B
IB
IB
I�B
I�B
I�B
I�B
I�B
I�B
J	B
J�B
J�B
J�B
J�B
J�B
KB
LB
L�B
MB
MB
MB
NB
M�B
M�B
M�B
M�B
M�B
NB
NB
M�B
OB
O(B
OB
OB
OBB
P.B
QB
Q B
QB
RB
RB
R B
R B
RB
RB
R B
R B
RB
SB
S&B
R�B
SB
SB
SB
S&B
S&B
T,B
T,B
T�B
T�B
T�B
T�B
UB
UB
UB
U2B
V9B
V9B
W?B
W?B
XEB
X+B
X+B
XEB
X_B
X+B
Y1B
YKB
Y1B
Z7B
Z7B
Z7B
ZQB
ZkB
ZkB
[=B
[WB
[WB
\CB
\]B
\]B
]IB
\xB
]dB
^OB
^OB
^OB
_pB
_VB
_pB
_pB
`vB
`vB
`vB
`\B
`BB
`BB
`\B
`\B
abB
abB
abB
a|B
bhB
bNB
b�B
b�B
bhB
bhB
bhB
bhB
c�B
cnB
cnB
cnB
c�B
cnB
dtB
d�B
d�B
dtB
d�B
ezB
e�B
e�B
f�B
f�B
ffB
f�B
f�B
f�B
f�B
f�B
g�B
g�B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
i�B
i�B
iyB
i�B
jB
i�B
i�B
i�B
j�B
j�B
j�B
j�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
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
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
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
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
xB
w�B
w�B
w�B
w�B
x�B
xB
w�B
x�B
x�B
y	B
y	B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
{B
z�B
z�B
z�B
z�B
z�B
{B
z�B
|B
|111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.07(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201705020036252017050200362520170502003625201806221312402018062213124020180622131240201804050714012018040507140120180405071401  JA  ARFMdecpA19c                                                                20170428093511  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20170428003523  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20170428003525  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20170428003525  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20170428003526  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20170428003526  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20170428003526  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20170428003526  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20170428003526  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20170428003526                      G�O�G�O�G�O�                JA  ARUP                                                                        20170428010712                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20170428153526  CV  JULD            G�O�G�O�F��                JM  ARCAJMQC2.0                                                                 20170501153625  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20170501153625  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404221401  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622041240  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116211516                      G�O�G�O�G�O�                