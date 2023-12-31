CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-07-11T00:35:24Z creation;2016-07-11T00:35:26Z conversion to V3.1;2019-12-19T08:36:00Z update;     
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
_FillValue                 �  I(   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `|   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �x   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �\   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �<   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �T   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �p   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �p   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �p   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �p   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �    HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �@   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �P   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �T   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �d   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �h   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �l   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �pArgo profile    3.1 1.2 19500101000000  20160711003524  20200115101516  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  I2_0576_015                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @׺vβ 1   @׺v��� @<..��2��dy���1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @9��@�  @�  A��A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr�Ct�Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8y�D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�C3DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�3D�C3D؀ D�� D�  D�@ D�|�D�� D���D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��fD��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @5�@{�@�A z�A�HA>�HA^�HA~�HA�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�B�RB�RB�RB�RB'�RB/�RB7�RB?�RBG�RBO�RBW�RB_�RBg�RBo�RBw�RB�RB��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cr�Ct�Cu�Cw�Cy�C{�C}�C�C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
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
��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D {�D ��D!{�D!��D"{�D"��D#{�D#��D${�D$��D%{�D%��D&{�D&��D'{�D'��D({�D(��D){�D)��D*{�D*��D+{�D+��D,{�D,��D-{�D-��D.{�D.��D/{�D/��D0{�D0��D1{�D1��D2{�D2��D3{�D3��D4{�D4��D5{�D5��D6{�D6��D7{�D7��D8uD8��D9{�D9��D:{�D:��D;{�D;��D<{�D<��D={�D=��D>{�D>��D?{�D?��D@{�D@��DA{�DA��DB{�DB��DC{�DC��DD{�DD��DE{�DE��DF{�DF��DG{�DG��DH{�DH��DI{�DI��DJ{�DJ��DK{�DK��DL{�DL��DM{�DM��DN{�DN��DO{�DO��DP{�DP��DQ{�DQ��DR{�DR��DS{�DS��DT{�DT��DU{�DU��DV{�DV��DW{�DW��DX{�DX��DY{�DY��DZ{�DZ��D[{�D[��D\{�D\��D]{�D]��D^{�D^��D_{�D_��D`{�D`��Da{�Da��Db{�Db��Dc{�Dc��Dd{�Dd��De{�De��Df{�Df��Dg{�Dg��Dh{�Dh��Di{�Di��Dj{�Dj��Dk{�Dk��Dl{�Dl��Dm{�Dm��Dn{�Dn��Do{�Do��Dp{�Dp��Dq{�Dq��Dr{�Dr��Ds{�Ds��Dt{�Dt��Du{�Du��Dv{�Dv��Dw{�Dw��Dx{�Dx��Dy{�Dy��Dz{�Dz��D{{�D{��D|{�D|��D}{�D}��D~{�D~��D{�D��D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D½�D���D�=�D�}�Dý�D���D�@�D�}�DĽ�D���D�=�D�}�DŽ�D���D�=�D�}�Dƽ�D���D�=�D�}�Dǽ�D���D�=�D�}�DȽ�D���D�=�D�}�Dɽ�D���D�=�D�}�Dʽ�D���D�=�D�}�D˽�D���D�=�D�}�D̽�D���D�=�D�}�Dͽ�D���D�=�D�}�Dν�D���D�=�D�}�DϽ�D���D�=�D�}�Dн�D���D�=�D�}�Dѽ�D���D�=�D�}�Dҽ�D���D�=�D�}�Dӽ�D���D�=�D�}�DԽ�D���D�=�D�}�Dս�D���D�=�D�}�Dֽ�D���D�=�D�}�D׽�D� �D�@�D�}�Dؽ�D���D�=�D�z�Dٽ�D���D�=�D�}�Dڽ�D���D�=�D�}�D۽�D���D�=�D�}�Dܽ�D���D�=�D�}�Dݽ�D���D�=�D�}�D޽�D���D�=�D�}�D߽�D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D���D���D�=�D�}�D��D���D�=�D�z�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D��)D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�{A�{A�oA��A��A��A��A��A�1A�AÛ�AA�jA�K�A�{A���A�ĜA��jA��-A���A�r�A�ƨA��A�`BA�S�A�bNA�ĜA�A��TA�5?A���A�1'A��/A�5?A��7A��wA���A��A���A�ZA�ffA�A�E�A��hA�ĜA�C�A�v�A�5?A�;dA���A�33A��hA�$�A�JA�-A��hA��#A�bNA�dZA�1'A�1A�|�A�M�A�A�I�A��#A��A�ĜA�bA&�A~z�A}�A}\)A|5?A{C�Ay�Ay?}Aw��Au�At��AtA�As+Ar1'Aq��ApM�An�Am�wAl�+AkS�AjbNAhjAg�-Af��AfĜAfffAfAe��Adz�AcXAbĜAbJA`��A_+A^�A]�A\�RA[��A[|�AZ�uAY\)AW��AU�AT�jAS"�AR  AQ/AOp�AN�/AN1AL~�AJ�AI�AI�7AH��AH��AH��AHVAG��AG��AGp�AF�AE�ACS�AB5?AA�AAXAA7LA@��A@=qA?l�A?�A>��A>�DA>�A=hsA<�jA;��A;�A:~�A:VA8ĜA7/A6$�A5;dA5%A4��A4-A2�`A1�A1C�A0$�A.��A.ĜA.ĜA.�jA-�A,�yA,�jA,ZA,5?A+�A+oA*^5A)t�A(��A&~�A%`BA$��A$ffA$  A#C�A"�A"$�A!�FA   AbNA��A��AdZAr�AQ�A=qA(�A{A�mA��A�!AE�A�wAVA��AM�A�;AS�A��A��Ap�AO�AA�mAr�AS�A�RAbNA�A/A
{A	;dA��A9XA��A�A�AȴA��A^5A5?AbA�;A�A�A�;AĜA�@�\)@�=q@�@���@��H@�?}@�j@��
@�|�@�S�@�"�@���@�I�@�@��y@��T@�/@�@�9X@��;@���@�^5@�D@���@�F@ᙚ@�b@���@�~�@���@��@�ȴ@���@�?}@��/@ו�@�+@֗�@�x�@�?}@�Z@��@�C�@�J@́@�&�@��@�Q�@˕�@���@�E�@ɲ-@�V@�t�@�Ĝ@�j@��m@�S�@�+@�+@�+@�"�@�o@�hs@��P@���@���@��D@�(�@��@�=q@�I�@���@�@���@���@���@�K�@��+@��@�7L@�K�@�5?@�@��@�bN@���@�~�@�{@�O�@���@�A�@���@�
=@�ff@�`B@�Z@��@�l�@�33@�@���@�v�@�-@��#@�Ĝ@�A�@���@�~�@�5?@�@���@���@��h@��@�X@�?}@�A�@���@�^5@�5?@�{@��-@��@�9X@��@��F@��P@�;d@�ff@�@���@�x�@�7L@��@��@��P@��@��@�7L@�V@���@��u@��
@�t�@�o@���@�ff@�=q@�J@��@���@�@��^@��-@���@���@��h@�O�@�%@��@�Q�@�b@��m@�ƨ@���@�"�@��H@��R@��\@�~�@�M�@��h@��@���@�Ĝ@�j@�Q�@�9X@��
@�|�@�;d@�33@���@�=q@��T@���@�x�@�O�@��@���@���@���@��D@�(�@�b@��m@��;@��
@�ƨ@���@���@�\)@�"�@�"�@�o@���@��H@�ȴ@�n�@��h@��h@���@�@��@�`B@���@�%@���@���@��@��u@�r�@�I�@�9X@�1'@� �@��@�1@�@~5?@}�@|j@{��@{�
@{�
@{�
@{ƨ@{��@{�@{t�@z~�@y��@y7L@x��@x��@xQ�@x �@xb@w�@w��@w;d@v�+@v5?@u�-@up�@t��@tZ@s�m@s�
@sƨ@s�@s33@s"�@s@r�H@r��@r-@rJ@q��@qG�@p1'@o|�@o�@n�+@n5?@n{@n$�@m��@m@mp�@l�@l�j@l�D@l(�@k�
@k��@k��@k��@kS�@j��@j��@j-@i��@ix�@h��@hbN@h  @g�@fȴ@e�T@e`B@eV@d��@eV@d��@d9X@dZ@d�D@dI�@d1@cdZ@c33@c@c"�@cƨ@cS�@b�@b~�@bM�@b=q@b�@a�#@a��@a��@ax�@aX@a7L@a%@`�`@`b@_|�@_;d@^�y@^�R@^�R@^��@^@]�h@]?}@\j@[��@[�
@[�@[dZ@[S�@[C�@[33@[@Z��@Z�@Y�^@Y��@Y��@Yx�@Yhs@X��@X��@X�9@X�u@XQ�@X  @W�w@Wl�@W;d@W;d@W+@V��@Vff@V{@U��@U@U��@U`B@U`B@UO�@U/@T��@S��@S�@So@RM�@Qx�@P�@P1'@P �@O��@O+@N��@NV@NV@NE�@NV@NE�@N$�@M@M�@MO�@L��@L�@L�@L�D@LI�@L(�@L�@L1@K��@K�m@K�m@K�F@Ko@J�\@J~�@J=q@J-@J�@J�@JJ@I7L@HĜ@H�u@HbN@HA�@H �@H  @G�@G�;@G�w@G|�@G\)@G\)@G;d@F��@F��@FE�@E�T@EO�@D�/@D�@D�D@Dj@DZ@DI�@D9X@D1@C33@B-@A��@A��@A�7@@��@@��@@r�@@Q�@@1'@@  @?|�@?K�@?+@>�@>E�@=�@=��@=��@=?}@<�D@<I�@<I�@<�@;��@;t�@;dZ@;33@:�H@:��@:M�@:�@:J@:J@9�#@9��@9x�@97L@8��@8��@8�@81'@8b@8b@7�@7��@7�@7l�@7K�@7;d@7+@7�@6��@6�@6V@6{@5��@5�@4��@4�@4�D@4Z@41@3�m@3��@3�@3�@3dZ@333@2�H@2�!@2=q@1��@1X@0��@0�`@0Ĝ@0�@0Q�@0A�@01'@/�@/�;@/�@/K�@/�@.��@.�y@.�@.�R@-�T@-`B@-�@,�/@,�@,z�@,(�@+��@+�m@+�
@+��@+�@+C�@*��@*~�@*n�@*M�@*J@)�#@)�^@)��@)x�@)7L@(�`@(bN@(b@'�@'��@'�w@'�P@'K�@'K�@';d@';d@'+@'
=@'
=@&��@&�y@&�R@&v�@&ff@&E�@&{@%V@$�@$�/@$�/@$��@$��@$��@$�j@$�j@$�@$z�@#�m@#�F@#��@#S�@#33@#o@"�@"�H@"��@"��@"�\@!�#@!x�@!�@ �9@ �@ r�@ Q�@ A�@ 1'@ 1'@ 1'@ 1'@  �@  �@   @�w@�P@l�@;d@��@��@�y@�+@5?@�@p�@/@��@Z@(�@�@��@�
@��@t�@S�@"�@�H@�!@�!@��@^5@�#@x�@7L@�@��@1'@b@b@�;@��@l�@K�@;d@��@�y@�y@�R@v�@v�@v�@�+@�+@�+@E�@E�@E�@5?@@�T@�-@��@�h@�@p�@O�@�@��@��@�D@z�@I�@�@��@ƨ@��@dZ@S�@33@��@�\@~�@=q@�@��@��@X@&�@��@��@��@��@��@��@��@Ĝ@�9@��@�u@bN@ �@b@�@��@�w@�@�P@+@��@E�@5?@5?@$�@$�@{@@�@�@��@��@�h@�@O�@�@��@Z@9X@(�@1@��@��@�m@�
@ƨ@�F@��@��@�@t�@33@@
��@
�\@
n�@
=q@	��@	��@	x�@	hs11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�{A�{A�oA��A��A��A��A��A�1A�AÛ�AA�jA�K�A�{A���A�ĜA��jA��-A���A�r�A�ƨA��A�`BA�S�A�bNA�ĜA�A��TA�5?A���A�1'A��/A�5?A��7A��wA���A��A���A�ZA�ffA�A�E�A��hA�ĜA�C�A�v�A�5?A�;dA���A�33A��hA�$�A�JA�-A��hA��#A�bNA�dZA�1'A�1A�|�A�M�A�A�I�A��#A��A�ĜA�bA&�A~z�A}�A}\)A|5?A{C�Ay�Ay?}Aw��Au�At��AtA�As+Ar1'Aq��ApM�An�Am�wAl�+AkS�AjbNAhjAg�-Af��AfĜAfffAfAe��Adz�AcXAbĜAbJA`��A_+A^�A]�A\�RA[��A[|�AZ�uAY\)AW��AU�AT�jAS"�AR  AQ/AOp�AN�/AN1AL~�AJ�AI�AI�7AH��AH��AH��AHVAG��AG��AGp�AF�AE�ACS�AB5?AA�AAXAA7LA@��A@=qA?l�A?�A>��A>�DA>�A=hsA<�jA;��A;�A:~�A:VA8ĜA7/A6$�A5;dA5%A4��A4-A2�`A1�A1C�A0$�A.��A.ĜA.ĜA.�jA-�A,�yA,�jA,ZA,5?A+�A+oA*^5A)t�A(��A&~�A%`BA$��A$ffA$  A#C�A"�A"$�A!�FA   AbNA��A��AdZAr�AQ�A=qA(�A{A�mA��A�!AE�A�wAVA��AM�A�;AS�A��A��Ap�AO�AA�mAr�AS�A�RAbNA�A/A
{A	;dA��A9XA��A�A�AȴA��A^5A5?AbA�;A�A�A�;AĜA�@�\)@�=q@�@���@��H@�?}@�j@��
@�|�@�S�@�"�@���@�I�@�@��y@��T@�/@�@�9X@��;@���@�^5@�D@���@�F@ᙚ@�b@���@�~�@���@��@�ȴ@���@�?}@��/@ו�@�+@֗�@�x�@�?}@�Z@��@�C�@�J@́@�&�@��@�Q�@˕�@���@�E�@ɲ-@�V@�t�@�Ĝ@�j@��m@�S�@�+@�+@�+@�"�@�o@�hs@��P@���@���@��D@�(�@��@�=q@�I�@���@�@���@���@���@�K�@��+@��@�7L@�K�@�5?@�@��@�bN@���@�~�@�{@�O�@���@�A�@���@�
=@�ff@�`B@�Z@��@�l�@�33@�@���@�v�@�-@��#@�Ĝ@�A�@���@�~�@�5?@�@���@���@��h@��@�X@�?}@�A�@���@�^5@�5?@�{@��-@��@�9X@��@��F@��P@�;d@�ff@�@���@�x�@�7L@��@��@��P@��@��@�7L@�V@���@��u@��
@�t�@�o@���@�ff@�=q@�J@��@���@�@��^@��-@���@���@��h@�O�@�%@��@�Q�@�b@��m@�ƨ@���@�"�@��H@��R@��\@�~�@�M�@��h@��@���@�Ĝ@�j@�Q�@�9X@��
@�|�@�;d@�33@���@�=q@��T@���@�x�@�O�@��@���@���@���@��D@�(�@�b@��m@��;@��
@�ƨ@���@���@�\)@�"�@�"�@�o@���@��H@�ȴ@�n�@��h@��h@���@�@��@�`B@���@�%@���@���@��@��u@�r�@�I�@�9X@�1'@� �@��@�1@�@~5?@}�@|j@{��@{�
@{�
@{�
@{ƨ@{��@{�@{t�@z~�@y��@y7L@x��@x��@xQ�@x �@xb@w�@w��@w;d@v�+@v5?@u�-@up�@t��@tZ@s�m@s�
@sƨ@s�@s33@s"�@s@r�H@r��@r-@rJ@q��@qG�@p1'@o|�@o�@n�+@n5?@n{@n$�@m��@m@mp�@l�@l�j@l�D@l(�@k�
@k��@k��@k��@kS�@j��@j��@j-@i��@ix�@h��@hbN@h  @g�@fȴ@e�T@e`B@eV@d��@eV@d��@d9X@dZ@d�D@dI�@d1@cdZ@c33@c@c"�@cƨ@cS�@b�@b~�@bM�@b=q@b�@a�#@a��@a��@ax�@aX@a7L@a%@`�`@`b@_|�@_;d@^�y@^�R@^�R@^��@^@]�h@]?}@\j@[��@[�
@[�@[dZ@[S�@[C�@[33@[@Z��@Z�@Y�^@Y��@Y��@Yx�@Yhs@X��@X��@X�9@X�u@XQ�@X  @W�w@Wl�@W;d@W;d@W+@V��@Vff@V{@U��@U@U��@U`B@U`B@UO�@U/@T��@S��@S�@So@RM�@Qx�@P�@P1'@P �@O��@O+@N��@NV@NV@NE�@NV@NE�@N$�@M@M�@MO�@L��@L�@L�@L�D@LI�@L(�@L�@L1@K��@K�m@K�m@K�F@Ko@J�\@J~�@J=q@J-@J�@J�@JJ@I7L@HĜ@H�u@HbN@HA�@H �@H  @G�@G�;@G�w@G|�@G\)@G\)@G;d@F��@F��@FE�@E�T@EO�@D�/@D�@D�D@Dj@DZ@DI�@D9X@D1@C33@B-@A��@A��@A�7@@��@@��@@r�@@Q�@@1'@@  @?|�@?K�@?+@>�@>E�@=�@=��@=��@=?}@<�D@<I�@<I�@<�@;��@;t�@;dZ@;33@:�H@:��@:M�@:�@:J@:J@9�#@9��@9x�@97L@8��@8��@8�@81'@8b@8b@7�@7��@7�@7l�@7K�@7;d@7+@7�@6��@6�@6V@6{@5��@5�@4��@4�@4�D@4Z@41@3�m@3��@3�@3�@3dZ@333@2�H@2�!@2=q@1��@1X@0��@0�`@0Ĝ@0�@0Q�@0A�@01'@/�@/�;@/�@/K�@/�@.��@.�y@.�@.�R@-�T@-`B@-�@,�/@,�@,z�@,(�@+��@+�m@+�
@+��@+�@+C�@*��@*~�@*n�@*M�@*J@)�#@)�^@)��@)x�@)7L@(�`@(bN@(b@'�@'��@'�w@'�P@'K�@'K�@';d@';d@'+@'
=@'
=@&��@&�y@&�R@&v�@&ff@&E�@&{@%V@$�@$�/@$�/@$��@$��@$��@$�j@$�j@$�@$z�@#�m@#�F@#��@#S�@#33@#o@"�@"�H@"��@"��@"�\@!�#@!x�@!�@ �9@ �@ r�@ Q�@ A�@ 1'@ 1'@ 1'@ 1'@  �@  �@   @�w@�P@l�@;d@��@��@�y@�+@5?@�@p�@/@��@Z@(�@�@��@�
@��@t�@S�@"�@�H@�!@�!@��@^5@�#@x�@7L@�@��@1'@b@b@�;@��@l�@K�@;d@��@�y@�y@�R@v�@v�@v�@�+@�+@�+@E�@E�@E�@5?@@�T@�-@��@�h@�@p�@O�@�@��@��@�D@z�@I�@�@��@ƨ@��@dZ@S�@33@��@�\@~�@=q@�@��@��@X@&�@��@��@��@��@��@��@��@Ĝ@�9@��@�u@bN@ �@b@�@��@�w@�@�P@+@��@E�@5?@5?@$�@$�@{@@�@�@��@��@�h@�@O�@�@��@Z@9X@(�@1@��@��@�m@�
@ƨ@�F@��@��@�@t�@33@@
��@
�\@
n�@
=q@	��@	��@	x�@	hs11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B��B��B��B��B��B��B��BǮB�/B�5B�5B�BB�HB�NB�NB�NB�HB�5B��B��B��B�B9XB��B��B��B��B|�BT�B?}B"�B�B�BB�B�B�HB��B�B��B��B��B�oB�+B�Bv�Bn�B^5BS�BL�BB�B5?B-B �B�BDB+BB
��B
��B
�B
�fB
�BB
�B
��B
��B
��B
��B
��B
��B
�VB
�%B
z�B
t�B
jB
]/B
S�B
P�B
H�B
@�B
9XB
0!B
"�B
 �B
�B
oB

=B	��B	��B	�B	�B	�B	�B	�sB	�NB	�B	��B	��B	ǮB	�jB	�XB	�9B	�B	��B	��B	��B	��B	�bB	�B	z�B	l�B	dZB	`BB	S�B	N�B	H�B	A�B	8RB	1'B	.B	+B	(�B	'�B	&�B	%�B	'�B	1'B	1'B	&�B	�B	{B	oB	hB	bB	bB	VB	JB	
=B	
=B		7B	1B	+B	B	B��B��B��B��B�B�sB�ZB�NB�HB�;B�)B�
B��B��B��BɺB��B��BȴBB��B�wB�qB�jB�XB�?B�'B�B��B��B��B��B��B��B��B��B�oB�\B�7B�B�B�B|�B{�Bz�Bz�Bz�Bx�Bv�Bo�Bn�Bl�BjBhsBgmBffBdZBcTBbNB`BB_;B_;B^5B[#BW
BW
BS�BS�BR�BO�BM�BK�BJ�BI�BH�BG�BG�BG�BG�BG�BG�BG�BF�BE�BE�BA�B?}B9XB6FB5?B49B0!B1'B2-B49B5?B6FB6FB7LB:^B33B0!B-B,B+B)�B(�B&�B$�B#�B �B�B�B�B�B�B �B�B�B �B�B�B!�B!�B"�B#�B"�B"�B#�B$�B&�B&�B&�B&�B&�B&�B'�B(�B(�B(�B+B,B,B-B-B-B-B-B-B-B2-B49B6FB7LB6FB6FB6FB7LB;dB>wB>wB>wB>wBA�BC�BD�BD�BE�BI�BK�BL�BM�BO�BP�BP�BP�BR�BT�BVB[#B\)B]/B`BBcTBe`BffBhsBjBk�Bl�Bm�Bn�Bu�Bu�Bw�Bz�B{�B|�B}�B~�B� B� B�B�B�%B�JB�PB�VB�VB�bB�oB��B��B��B��B��B��B��B��B��B��B��B�B�B�!B�FB�^B�^B�^B�qB��BÖBƨBɺB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�#B�/B�5B�;B�;B�;B�BB�`B�sB�B�B�B�B�B�B��B��B��B��B��B��B��B��B	  B	B	B	B	B	%B	
=B	DB	PB	VB	VB	\B	hB	oB	�B	�B	�B	�B	�B	�B	 �B	"�B	$�B	(�B	+B	/B	1'B	33B	33B	5?B	5?B	6FB	8RB	8RB	9XB	:^B	<jB	<jB	=qB	=qB	=qB	?}B	@�B	A�B	C�B	C�B	D�B	D�B	E�B	E�B	F�B	G�B	G�B	M�B	R�B	W
B	YB	ZB	\)B	_;B	bNB	cTB	ffB	gmB	jB	k�B	m�B	o�B	r�B	u�B	v�B	w�B	v�B	w�B	{�B	|�B	}�B	~�B	� B	�B	�B	�B	�B	�%B	�1B	�7B	�=B	�PB	�VB	�hB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�?B	�FB	�RB	�^B	�dB	�qB	�wB	��B	��B	��B	��B	��B	��B	B	ŢB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�
B	�B	�B	�#B	�/B	�/B	�5B	�;B	�;B	�;B	�;B	�HB	�ZB	�`B	�mB	�mB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
1B
	7B
	7B
	7B

=B
DB
DB
DB
DB
DB
DB
JB
PB
\B
\B
bB
bB
bB
bB
bB
uB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
%�B
&�B
&�B
&�B
'�B
'�B
(�B
(�B
)�B
)�B
+B
+B
+B
+B
,B
,B
,B
-B
-B
.B
.B
.B
/B
/B
/B
/B
/B
0!B
0!B
0!B
0!B
1'B
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
2-B
33B
33B
33B
49B
5?B
5?B
5?B
5?B
6FB
6FB
6FB
7LB
7LB
7LB
7LB
7LB
8RB
8RB
8RB
9XB
9XB
:^B
:^B
:^B
:^B
;dB
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
>wB
>wB
>wB
?}B
?}B
?}B
@�B
@�B
@�B
@�B
@�B
@�B
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
C�B
D�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
I�B
I�B
I�B
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
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
Q�B
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
VB
W
B
W
B
W
B
W
B
W
B
W
B
W
B
XB
XB
XB
XB
YB
YB
ZB
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
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
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
bNB
bNB
bNB
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
dZB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
ffB
ffB
ffB
ffB
ffB
gmB
gmB
gmB
gm11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B��B��B��B��B�-B��B��B�RB�IBބB޸B��B�|B�hB�B�B�NB�vB�1B��B�oB��BB�B��B�
BǔB��B�aB[�BB�B%B \BBEB��B�cB��B�aB��B��B�~B��B�B�fB�9Bx�Bq�B_�BU�BO\BD�B6�B.�B"4BdB�B�B9B
��B
�8B
�B
�B
�bB
�	B
��B
�GB
��B
��B
��B
��B
��B
��B
|6B
v�B
l�B
^5B
T�B
R:B
J	B
A�B
;B
2B
#�B
"hB
B
�B
dB	��B	��B	�-B	�3B	�;B	�]B	��B	�B	�B	�MB	бB	�7B	�VB	��B	�tB	�B	�B	�XB	��B	��B	�oB	��B	|�B	n/B	e�B	bB	UB	P.B	J�B	CaB	9�B	1�B	.�B	+kB	)DB	(XB	'mB	&�B	(�B	2|B	33B	(�B	B	MB	�B	�B	B	hB	BB	�B	
�B	
�B	
	B		7B	1B	3B	'B��B��B�B��B��B�yB��B�B�4B��B�dB�EBևB� B�0B��B�)B��B��B�B� B��B�(B��B��B��B��B��B�KB��B��B�pB��B�kB��B��B��B�NB�rB��B��B�'B}<B|6B{0B{JB|B{dBxBpoBoiBm�BkBiBh$BgRBe`BdtBb�B`�B`B`�B`B\xBW�BW�BT�BUMBTaBQ BN�BL~BKxBJ�BIBG�BG�BHBG�BHBH1BH�BG�BGzBGzBC�BA B:*B6�B6FB5tB1AB1�B2�B4�B5�B6�B72B9	B<6B4�B0�B-�B,qB+kB*B)�B'�B&�B%�B!�B 'B �B �B vB �B!�B 'B vB!HB BB �B"4B"hB#�B$tB#�B$�B$�B%�B'mB'8B'8B'mB'�B'�B(�B)�B)�B*eB,�B,qB,�B-wB-CB-)B-)B-]B-�B.cB3�B5ZB6�B7�B6�B7B7fB8�B<jB>�B>�B>�B?HBBuBD3BEBE�BF�BJ�BL0BMjBNpBP�BQ�BQ�BQ�BSuBUgBV�B[�B\�B^BaBc�Be�Bf�Bh�Bj�Bk�Bl�BnBoiBv+Bv`Bx�B{0B|6B}"B~(BB�4B�OB�UB��B�B��B��B��B��B� B�B��B��B��B�B�CB�B�B�:B�,B��B�yB��B��B�B��B��B��B��B��B��B��B��B�	B��B��B�B�B�B�B��B�B�B�B�(B�NB�TB�MB�9B�EB�KB�QBیB�~B�jB�pB�pBߊB��B��B�B�B��B�B��B��B�B��B�B�DB�xB�]B�HB�HB�.B	 OB	 B	-B	GB	gB	tB	
XB	xB	jB	�B	�B	vB	�B	�B	�B	�B	�B	�B	�B	�B	!-B	#nB	$�B	(�B	+B	/OB	1[B	3�B	3MB	5tB	5�B	6�B	8lB	8lB	9rB	:xB	<�B	<�B	=�B	=�B	=�B	@ B	@�B	A�B	C�B	C�B	D�B	D�B	E�B	E�B	F�B	G�B	H1B	N"B	S&B	W$B	YKB	ZkB	\CB	_pB	b�B	cnB	f�B	g�B	j�B	k�B	m�B	pB	r�B	u�B	v�B	w�B	v�B	xB	|B	}B	~B	.B	�OB	� B	�;B	�aB	��B	�tB	�fB	�lB	�XB	��B	�pB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�&B	�B	�B	�$B	�0B	�6B	�qB	�cB	�[B	�tB	�zB	�lB	��B	��B	�qB	��B	��B	��B	��B	��B	��B	��B	B	��B	��B	��B	��B	��B	�B	�B	�B	��B	�B	�B	� B	�B	�B	�[B	�,B	�B	�B	�2B	�B	�9B	�MB	�?B	�_B	�eB	�WB	�IB	�IB	�jB	�;B	�pB	�VB	�VB	�|B	�B	�zB	�B	�B	�B	�B	��B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�6B	�VB	�BB	�B
 B
;B
[B
aB
MB
B
B
B
3B
MB
9B
?B
_B
_B
fB
	RB
	RB
	lB

XB
^B
DB
DB
^B
^B
xB
~B
�B
vB
vB
bB
}B
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
B
!B
#B
#�B
#�B
$&B
$�B
$�B
$�B
$�B
%B
&B
'B
'B
'B
($B
($B
)*B
)B
*KB
*0B
+6B
+B
+B
+6B
,=B
,=B
,"B
-CB
-)B
.IB
./B
.B
/B
/OB
/OB
/5B
/OB
0;B
0;B
0;B
0;B
1[B
1AB
1AB
1[B
1[B
2aB
2GB
2GB
2-B
2aB
2GB
2GB
3hB
3hB
3�B
4nB
5tB
5ZB
5ZB
5tB
6`B
6zB
6`B
7fB
7fB
7fB
7�B
7fB
8�B
8�B
8�B
9�B
9rB
:^B
:�B
:xB
:�B
;dB
;�B
;B
;B
;B
;B
<�B
<�B
<�B
<�B
<�B
<�B
>�B
>�B
>�B
?�B
?�B
?�B
@�B
@�B
@�B
@�B
@�B
@�B
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
C�B
D�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
I�B
I�B
I�B
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
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
MB
MB
NB
NB
OB
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
OB
O�B
O�B
O�B
O�B
P�B
QB
QB
QB
Q B
R:B
RB
RB
R B
SB
S�B
TB
TB
T,B
T,B
TB
TB
TB
U2B
UB
UB
UB
UMB
UB
U2B
UB
V9B
VB
VB
W
B
W$B
W$B
W?B
W?B
W$B
W?B
XB
X+B
XEB
XEB
YB
YB
Z7B
ZB
Z7B
ZQB
ZB
ZB
Z7B
[WB
[WB
[WB
\CB
\)B
\)B
\CB
\CB
\CB
\CB
\CB
\)B
\]B
\CB
\CB
\CB
\]B
\]B
\]B
\]B
\CB
]IB
]IB
]IB
]IB
^OB
^jB
^OB
^OB
_VB
_VB
_;B
_;B
_;B
_;B
_;B
_;B
_VB
_;B
_;B
_pB
_VB
`\B
`BB
`vB
`\B
`BB
`\B
`vB
`vB
a|B
abB
bhB
bNB
bhB
bhB
bhB
bNB
bNB
bhB
b�B
bhB
bhB
cTB
cnB
cnB
cnB
d�B
dtB
dtB
dtB
ezB
e`B
e`B
e`B
ezB
e`B
e`B
ezB
e`B
ezB
e�B
ezB
f�B
f�B
f�B
f�B
f�B
g�B
g�B
gmB
gm11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<>�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.07(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201607150034502016071500345020160715003450201806221210502018062212105020180622121050201804050403092018040504030920180405040309  JA  ARFMdecpA19c                                                                20160711093505  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20160711003524  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20160711003524  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20160711003525  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20160711003525  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20160711003525  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20160711003525  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20160711003525  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20160711003526  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20160711003526                      G�O�G�O�G�O�                JA  ARUP                                                                        20160711012003                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20160711153652  CV  JULD            G�O�G�O�F�ӱ                JM  ARGQJMQC2.0                                                                 20160711153652  CV  JULD_LOCATION   G�O�G�O�F���                JM  ARGQJMQC2.0                                                                 20160711153652  CV  LATITUDE        G�O�G�O�A�v�                JM  ARGQJMQC2.0                                                                 20160711153652  CV  LONGITUDE       G�O�G�O��#ɺ                JM  ARCAJMQC2.0                                                                 20160714153450  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20160714153450  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404190309  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622031050  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115101516                      G�O�G�O�G�O�                