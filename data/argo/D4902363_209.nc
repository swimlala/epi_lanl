CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-02-13T00:35:28Z creation;2018-02-13T00:35:34Z conversion to V3.1;2019-12-19T07:49:30Z update;     
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
_FillValue                 �  I$   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `t   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �L   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �$   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �4   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ܼ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �L   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �L   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �L   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �L   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �,   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �0   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �@   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �D   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �H   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �LArgo profile    3.1 1.2 19500101000000  20180213003528  20200115121516  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0576_209                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�K�� 1   @�K�o� @:�($x�d_�+1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A`  A�  A�  A�  A�33A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BG��BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D��D�3D	� D
fD
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� DcfDc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D�|�D�� D���D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D�3D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�(�@�@�A�HA>�HA^�HA~�HA�p�A�p�A���A�p�A�p�A�p�A�p�A�p�B�RB�RB�RB�RB'�RB/�RB7�RB?�RBGQ�BO�RBW�RB_�RBg�RBo�RBw�RB�RB��)B��)B��)B��)B��)B��)B��)B��)B��)B�\B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
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
D {�D ��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D�RD�D	{�D
�D
{�D
��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D {�D ��D!{�D!��D"{�D"��D#{�D#��D${�D$��D%{�D%��D&{�D&��D'{�D'��D({�D(��D){�D)��D*{�D*��D+{�D+��D,{�D,��D-{�D-��D.{�D.��D/{�D/��D0{�D0��D1{�D1��D2{�D2��D3{�D3��D4{�D4��D5{�D5��D6{�D6��D7{�D7��D8{�D8��D9{�D9��D:{�D:��D;{�D;��D<{�D<��D={�D=��D>{�D>��D?{�D?��D@{�D@��DA{�DA��DB{�DB��DC{�DC��DD{�DD��DE{�DE��DF{�DF��DG{�DG��DH{�DH��DI{�DI��DJ{�DJ��DK{�DK��DL{�DL��DM{�DM��DN{�DN��DO{�DO��DP{�DP��DQ{�DQ��DR{�DR��DS{�DS��DT{�DT��DU{�DU��DV{�DV��DW{�DW��DX{�DX��DY{�DY��DZ{�DZ��D[{�D[��D\{�D\��D]{�D]��D^{�D^��D_{�D_��D`{�D`��Da{�Da��Db{�Dc�Dc{�Dc��Dd{�Dd��De{�De��Df{�Df��Dg{�Dg��Dh{�Dh��Di{�Di��Dj{�Dj��Dk{�Dk��Dl{�Dl��Dm{�Dm��Dn{�Dn��Do{�Do��Dp{�Dp��Dq{�Dq��Dr{�Dr��Ds{�Ds��Dt{�Dt��Du{�Du��Dv{�Dv��Dw{�Dw��Dx{�Dx��Dy{�Dy��Dz{�Dz��D{{�D{��D|{�D|��D}{�D}��D~{�D~��D{�D��D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�@�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D½�D���D�=�D�}�Dý�D���D�=�D�}�DĽ�D���D�=�D�}�DŽ�D���D�=�D�}�Dƽ�D���D�=�D�}�Dǽ�D���D�=�D�}�DȽ�D���D�=�D�}�Dɽ�D���D�=�D�}�Dʽ�D���D�=�D�}�D˽�D���D�=�D�}�D̽�D���D�=�D�}�Dͽ�D���D�=�D�}�Dν�D���D�=�D�}�DϽ�D���D�=�D�}�Dн�D���D�=�D�}�Dѽ�D���D�=�D�}�Dҽ�D���D�=�D�}�Dӽ�D���D�=�D�}�DԽ�D���D�=�D�}�Dս�D���D�=�D�}�Dֽ�D���D�=�D�z�D׽�D���D�=�D�}�Dؽ�D���D�=�D�}�Dٽ�D���D�=�D�}�Dڽ�D���D�=�D�}�D۽�D���D�=�D�}�Dܽ�D���D�=�D�}�Dݽ�D���D�=�D�}�D޽�D���D�=�D�}�D߽�D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�@�D��D��D���D�=�D�}�D��D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D�ʏ1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A��A��A��A��A��+A��+A��+A��7A��+A��7A��7A��7A��DA��DA��DA��DA��+A��A�|�A��A�t�A�A�A��wA���A���A��\A�v�A�^5A�S�A�Q�A�K�A�A�A�=qA�5?A�&�A��A��mA��/A���A�z�A�C�A�5?A�9XA��A��yA�ȴA��A�;dA��`A��!A�hsA��yA�ffA�/A�
=A�r�A�ȴA��+A��A��uA�bNA��A��PA��7A�bNA���A���A���A���A�G�A� �A�  A��A��`A�r�A�ƨA���A�-A�t�A��-A��A�?}A��uA�+A�{A��A��hA�9XA��A�jA��TA�bA��A�A~I�A}A|�`A{�hAvn�At$�As��Ap�/An�DAk�
Aj�9Ai�wAi;dAh�Ag�Af�DAe�Ac
=Aa�FA`I�A_A^�9A^(�A]G�A\�`A\�DA\-A[�AZ�yAZ9XAY|�AX��AW�#AV��AS��AOANM�ANbAMAMx�AMAL5?AKƨAK
=AH��AG�AFv�AE��AEC�AD�AD�uACXAB~�AA�;AA33A@��A@n�A@1'A?�wA>�A>n�A=ƨA=oA<�/A;��A;
=A:Q�A9��A9G�A9�A8ĜA8~�A8 �A7;dA61A4�HA3��A3�A3ƨA3hsA2E�A2{A1��A0�/A/%A-O�A,��A,1'A*jA)O�A(ȴA(^5A'�;A'hsA'?}A&�HA&�9A&JA%"�A$1'A#��A#&�A"�uA!�mA!&�A �A bNA�7A�Ap�A7LA%A��A�A�/A��A�!A�DAn�Al�Av�A-A
=AƨA��AO�AȴA1A�A�/A�uA�7A�DA9XA�FA�A{A��AbA
�RA
z�A
I�A�yA�A��AXAoA��A��AAffA�A�wAK�A��A��AbNAG�A   @���@�{@�/@�Q�@��@��;@���@���@�V@��@��9@�@���@��/@�Q�@��@�C�@��@�ƨ@�p�@�;d@�R@�Ĝ@�v�@���@�ȴ@�t�@�  @Ұ!@Ѳ-@Ͼw@Χ�@�ff@�X@̴9@�dZ@�~�@�V@�dZ@���@őh@�O�@�j@� �@��@��
@öF@î@Ý�@�|�@�@�1@�@�A�@��@��j@�z�@�9X@��@��F@�S�@�M�@��#@��@�&�@��@��@��P@���@���@���@��@��@���@���@�?}@�V@���@��m@���@��@�@�?}@�Ĝ@��@��@�V@�5?@��@��@��7@�&�@�|�@�v�@�ff@��@���@�O�@��D@�(�@�t�@�ff@�p�@���@�1'@���@��w@��F@��@�t�@�\)@�S�@�33@�33@�o@���@�^5@�-@�J@��-@�x�@�hs@�X@�G�@��@�r�@��
@�l�@�"�@��H@���@��+@��@��@�I�@� �@��@�v�@�`B@�Ĝ@��j@���@�bN@�9X@��w@��@�x�@�G�@�7L@���@�I�@�bN@�(�@��m@��@�\)@�o@��@���@���@��+@�^5@��@���@�J@�{@�@��-@���@��@�`B@�G�@�G�@�G�@�X@��7@��@��j@�j@��@���@��@��@�|�@�|�@�l�@�\)@�\)@�S�@�K�@�\)@�33@�ȴ@�ff@�5?@�{@��@��7@���@��/@��9@�9X@��m@���@�t�@�t�@�t�@�l�@��@���@���@�~�@�v�@�^5@�M�@�=q@��@��@�?}@��@�I�@�(�@�b@�@��@;d@~��@~$�@}��@}V@|z�@z�@y%@xQ�@w�@w�@wK�@v��@v�+@v��@v�+@v$�@uO�@t�@tj@s�
@s�@sdZ@sC�@so@r�!@rM�@rJ@q��@q�@q�@q�#@qhs@q�@p��@pr�@pA�@p �@pb@o�;@n�y@m��@m/@l�j@lZ@k�
@kdZ@kC�@kdZ@kt�@k�@kdZ@kC�@kC�@kC�@k33@j�H@j�\@j^5@jJ@ix�@i&�@i7L@i%@h��@i%@h�`@h�u@h1'@gl�@f�@e�T@eO�@d(�@c��@cƨ@c��@cdZ@b�H@b~�@b~�@b~�@b�\@b~�@a�@a��@a��@a��@a&�@`bN@_�w@^�R@^@]��@]�-@]`B@\�j@\�@[o@Zn�@ZM�@Z�@Y��@Yhs@Y%@X�9@XbN@XQ�@X  @W;d@Vȴ@V��@V�+@Vv�@V$�@U@U��@U�h@U�@U�@TZ@S��@S��@SdZ@S@R�\@RM�@Q�^@P��@P�@PQ�@Pb@O�;@O�w@O|�@O�@N��@N�+@NV@N@M�T@M�-@M��@M��@M�h@Mp�@MO�@M`B@M/@Lj@Kƨ@Kt�@K�@KS�@KC�@Ko@J�\@J=q@J=q@JM�@I�#@IG�@H��@H��@Hr�@HA�@G��@G��@G|�@Gl�@Gl�@G+@Fȴ@F�R@F�+@F5?@E@E��@E��@Ep�@Ep�@E/@D��@D��@D1@CS�@B�@B��@B�!@BJ@A�^@Ax�@A7L@@��@@�u@@1'@?�@?�;@?��@>�@>��@>v�@>5?@>{@>@=�T@=�-@=��@=�@<��@<j@;��@;dZ@;C�@;"�@:��@:~�@:-@9�^@9&�@8�`@8�u@8Q�@8  @7+@6�@6ȴ@6�R@6�+@65?@5�@5�-@5�@5O�@4��@4�/@4��@4Z@4(�@41@3�
@3��@3t�@3C�@2�H@2n�@2-@1��@1��@1X@0��@0��@0Ĝ@0�9@0�u@0Q�@0 �@/�;@/��@/��@/|�@/K�@.�y@.ȴ@.�+@.ff@.V@.5?@-�@-��@-�h@-?}@-?}@-?}@-/@-�@,��@,�@,Z@,�@+ƨ@+t�@+33@*�@*��@*^5@*�@)�#@)��@)G�@(��@(Q�@(b@'��@&��@&$�@%@%��@%�h@%�h@%p�@%O�@%V@$�@$��@$�@$j@$9X@#�m@#�@#33@"��@"�!@"��@"�\@"~�@"n�@"^5@"-@"J@"J@!�@!��@!hs@!G�@!�@ �`@ ��@ Q�@�;@|�@\)@;d@�@v�@@�T@�-@�@p�@O�@��@z�@9X@1@�m@ƨ@��@�@C�@"�@��@~�@M�@��@�@�@�@��@��@��@X@G�@7L@�@%@�9@Q�@A�@A�@ �@��@�w@�@��@�P@K�@�y@ȴ@�+@{@�@�T@��@@�-@�@p�@O�@/@��@�j@z�@j@9X@�@�m@�
@S�@33@"�@"�@o@@��@��@^5@=q@J@�@�#@X@Q�@ �@��@;d@�@
=@
=@��@�R@�+@�+@v�@v�@V@E�@@@�-@�-@��@`B@/@�@V@�D@9X@�m@�F@�F@��@�@t�@t�@dZ@dZ@S�@@
�H@
�H@
�H@
��@
��@
��@
�!@
�\@
^5@
-@	�#@	��@	��@	�7@	hs@	X@	�@Ĝ@�u@bN@Q�@Q�@Q�@A�@1'@  @�@�@�;@�@�@��@��@|�@l�@
=@�@��@v�@E�@5?@$�@�@@�h@�@p�@`B@O�@O�@/@V@�@�/@�j@��@��@�D@(�@�@1@��@�
@�
@ƨ@t�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A��A��A��A��A��+A��+A��+A��7A��+A��7A��7A��7A��DA��DA��DA��DA��+A��A�|�A��A�t�A�A�A��wA���A���A��\A�v�A�^5A�S�A�Q�A�K�A�A�A�=qA�5?A�&�A��A��mA��/A���A�z�A�C�A�5?A�9XA��A��yA�ȴA��A�;dA��`A��!A�hsA��yA�ffA�/A�
=A�r�A�ȴA��+A��A��uA�bNA��A��PA��7A�bNA���A���A���A���A�G�A� �A�  A��A��`A�r�A�ƨA���A�-A�t�A��-A��A�?}A��uA�+A�{A��A��hA�9XA��A�jA��TA�bA��A�A~I�A}A|�`A{�hAvn�At$�As��Ap�/An�DAk�
Aj�9Ai�wAi;dAh�Ag�Af�DAe�Ac
=Aa�FA`I�A_A^�9A^(�A]G�A\�`A\�DA\-A[�AZ�yAZ9XAY|�AX��AW�#AV��AS��AOANM�ANbAMAMx�AMAL5?AKƨAK
=AH��AG�AFv�AE��AEC�AD�AD�uACXAB~�AA�;AA33A@��A@n�A@1'A?�wA>�A>n�A=ƨA=oA<�/A;��A;
=A:Q�A9��A9G�A9�A8ĜA8~�A8 �A7;dA61A4�HA3��A3�A3ƨA3hsA2E�A2{A1��A0�/A/%A-O�A,��A,1'A*jA)O�A(ȴA(^5A'�;A'hsA'?}A&�HA&�9A&JA%"�A$1'A#��A#&�A"�uA!�mA!&�A �A bNA�7A�Ap�A7LA%A��A�A�/A��A�!A�DAn�Al�Av�A-A
=AƨA��AO�AȴA1A�A�/A�uA�7A�DA9XA�FA�A{A��AbA
�RA
z�A
I�A�yA�A��AXAoA��A��AAffA�A�wAK�A��A��AbNAG�A   @���@�{@�/@�Q�@��@��;@���@���@�V@��@��9@�@���@��/@�Q�@��@�C�@��@�ƨ@�p�@�;d@�R@�Ĝ@�v�@���@�ȴ@�t�@�  @Ұ!@Ѳ-@Ͼw@Χ�@�ff@�X@̴9@�dZ@�~�@�V@�dZ@���@őh@�O�@�j@� �@��@��
@öF@î@Ý�@�|�@�@�1@�@�A�@��@��j@�z�@�9X@��@��F@�S�@�M�@��#@��@�&�@��@��@��P@���@���@���@��@��@���@���@�?}@�V@���@��m@���@��@�@�?}@�Ĝ@��@��@�V@�5?@��@��@��7@�&�@�|�@�v�@�ff@��@���@�O�@��D@�(�@�t�@�ff@�p�@���@�1'@���@��w@��F@��@�t�@�\)@�S�@�33@�33@�o@���@�^5@�-@�J@��-@�x�@�hs@�X@�G�@��@�r�@��
@�l�@�"�@��H@���@��+@��@��@�I�@� �@��@�v�@�`B@�Ĝ@��j@���@�bN@�9X@��w@��@�x�@�G�@�7L@���@�I�@�bN@�(�@��m@��@�\)@�o@��@���@���@��+@�^5@��@���@�J@�{@�@��-@���@��@�`B@�G�@�G�@�G�@�X@��7@��@��j@�j@��@���@��@��@�|�@�|�@�l�@�\)@�\)@�S�@�K�@�\)@�33@�ȴ@�ff@�5?@�{@��@��7@���@��/@��9@�9X@��m@���@�t�@�t�@�t�@�l�@��@���@���@�~�@�v�@�^5@�M�@�=q@��@��@�?}@��@�I�@�(�@�b@�@��@;d@~��@~$�@}��@}V@|z�@z�@y%@xQ�@w�@w�@wK�@v��@v�+@v��@v�+@v$�@uO�@t�@tj@s�
@s�@sdZ@sC�@so@r�!@rM�@rJ@q��@q�@q�@q�#@qhs@q�@p��@pr�@pA�@p �@pb@o�;@n�y@m��@m/@l�j@lZ@k�
@kdZ@kC�@kdZ@kt�@k�@kdZ@kC�@kC�@kC�@k33@j�H@j�\@j^5@jJ@ix�@i&�@i7L@i%@h��@i%@h�`@h�u@h1'@gl�@f�@e�T@eO�@d(�@c��@cƨ@c��@cdZ@b�H@b~�@b~�@b~�@b�\@b~�@a�@a��@a��@a��@a&�@`bN@_�w@^�R@^@]��@]�-@]`B@\�j@\�@[o@Zn�@ZM�@Z�@Y��@Yhs@Y%@X�9@XbN@XQ�@X  @W;d@Vȴ@V��@V�+@Vv�@V$�@U@U��@U�h@U�@U�@TZ@S��@S��@SdZ@S@R�\@RM�@Q�^@P��@P�@PQ�@Pb@O�;@O�w@O|�@O�@N��@N�+@NV@N@M�T@M�-@M��@M��@M�h@Mp�@MO�@M`B@M/@Lj@Kƨ@Kt�@K�@KS�@KC�@Ko@J�\@J=q@J=q@JM�@I�#@IG�@H��@H��@Hr�@HA�@G��@G��@G|�@Gl�@Gl�@G+@Fȴ@F�R@F�+@F5?@E@E��@E��@Ep�@Ep�@E/@D��@D��@D1@CS�@B�@B��@B�!@BJ@A�^@Ax�@A7L@@��@@�u@@1'@?�@?�;@?��@>�@>��@>v�@>5?@>{@>@=�T@=�-@=��@=�@<��@<j@;��@;dZ@;C�@;"�@:��@:~�@:-@9�^@9&�@8�`@8�u@8Q�@8  @7+@6�@6ȴ@6�R@6�+@65?@5�@5�-@5�@5O�@4��@4�/@4��@4Z@4(�@41@3�
@3��@3t�@3C�@2�H@2n�@2-@1��@1��@1X@0��@0��@0Ĝ@0�9@0�u@0Q�@0 �@/�;@/��@/��@/|�@/K�@.�y@.ȴ@.�+@.ff@.V@.5?@-�@-��@-�h@-?}@-?}@-?}@-/@-�@,��@,�@,Z@,�@+ƨ@+t�@+33@*�@*��@*^5@*�@)�#@)��@)G�@(��@(Q�@(b@'��@&��@&$�@%@%��@%�h@%�h@%p�@%O�@%V@$�@$��@$�@$j@$9X@#�m@#�@#33@"��@"�!@"��@"�\@"~�@"n�@"^5@"-@"J@"J@!�@!��@!hs@!G�@!�@ �`@ ��@ Q�@�;@|�@\)@;d@�@v�@@�T@�-@�@p�@O�@��@z�@9X@1@�m@ƨ@��@�@C�@"�@��@~�@M�@��@�@�@�@��@��@��@X@G�@7L@�@%@�9@Q�@A�@A�@ �@��@�w@�@��@�P@K�@�y@ȴ@�+@{@�@�T@��@@�-@�@p�@O�@/@��@�j@z�@j@9X@�@�m@�
@S�@33@"�@"�@o@@��@��@^5@=q@J@�@�#@X@Q�@ �@��@;d@�@
=@
=@��@�R@�+@�+@v�@v�@V@E�@@@�-@�-@��@`B@/@�@V@�D@9X@�m@�F@�F@��@�@t�@t�@dZ@dZ@S�@@
�H@
�H@
�H@
��@
��@
��@
�!@
�\@
^5@
-@	�#@	��@	��@	�7@	hs@	X@	�@Ĝ@�u@bN@Q�@Q�@Q�@A�@1'@  @�@�@�;@�@�@��@��@|�@l�@
=@�@��@v�@E�@5?@$�@�@@�h@�@p�@`B@O�@O�@/@V@�@�/@�j@��@��@�D@(�@�@1@��@�
@�
@ƨ@t�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BhBhBhBhBhBhBhBhBhBhBhBhBhBhBhBhBbB\BVBPB
=BB��B  BBBB��B  BB  B  B��B��B��B��B��B��B�B�B�B�B�B�B�mB�`B�5B�B��B��BǮB�wB�'B�B��Bx�BP�B9XBB�BB�oB�%B�PBx�BhsBdZBp�Bo�Bk�BbNBG�BR�BR�BM�BA�B1'B1'B#�BJB
�B
�/B
�ZB
��B
��B
�/B
��B
��B
ŢB
�LB
�XB
��B
�DB
q�B
ffB
jB
l�B
\)B
F�B
�B
PB
 �B	��B	�B	�yB	�B	�B	�B	�mB	��B	�B	��B	�B	�!B	�B	��B	�B	��B	��B	��B	��B	��B	�uB	�PB	�1B	~�B	y�B	m�B	[#B	5?B	"�B	:^B	M�B	K�B	I�B	B�B	;dB	6FB	,B	�B	hB	#�B	�B	 �B	�B	�B	PB	
=B	VB	
=B	\B		7B	
=B	%B��B��B��B�B��B�B�mB�fB�mB�sB�B�fB�TB�5B��BȴBɺBȴB�B��B��B�}BƨB��B�9B��B��B�B��B�hB��B��B��B��B��B��B��B��B�oB�+B�1B�JB�1B�%B�B~�B�Bz�Bm�B]/BZBy�Bz�B{�B{�Bz�By�Bv�Br�Bo�BbNB]/Be`BZBQ�Be`BaHB\)BVBXBT�BT�BK�BG�BR�BK�BF�BA�BF�B0!B1'BE�B@�B2-B6FB=qB=qB;dB6FB1'B0!B33B7LB5?B2-B33B/B-B�B�B%�B.B+B+B.B+B!�B�B�B!�B �B�B�B"�B$�B$�B$�B�B�B�B�B�BoBBB�yB  BB�B�B{B�B!�B�B�B�B�B�B�B�B'�B'�B$�B-B.B/B/B/B.B+B"�B�B �B�B�B49B:^B9XB:^B9XB8RB49B9XB<jB<jB;dB<jB8RB7LB:^B;dB?}B<jB9XBG�BI�BI�BH�BF�BJ�BH�BF�BN�BO�BL�BS�BT�B]/B]/B\)BYBYBR�B\)BiyBffBhsBffBe`BiyBhsBgmBm�Bs�B{�B� B�B�B�B�B�B�B�B�%B�B�B�%B�7B�7B�7B�DB�VB�VB�PB�PB�7B�VB�{B��B��B��B��B��B��B��B��B��B��B��B�!B�XB�RB�LB�RB�?B�B�qBĜBŢBÖBƨB��B��B��B�B�
B�B�5B�NB�TB�ZB�`B�mB�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B	B		7B	VB	bB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	"�B	$�B	#�B	"�B	 �B	)�B	)�B	)�B	-B	/B	0!B	2-B	33B	49B	33B	6FB	<jB	=qB	>wB	>wB	?}B	@�B	?}B	>wB	<jB	C�B	E�B	J�B	K�B	L�B	K�B	L�B	K�B	M�B	O�B	S�B	VB	R�B	XB	_;B	cTB	e`B	ffB	gmB	m�B	m�B	m�B	k�B	m�B	q�B	v�B	v�B	z�B	}�B	~�B	� B	� B	�B	�B	�B	�%B	�+B	�%B	�B	�B	�1B	�=B	�VB	�hB	�oB	�hB	�\B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�-B	�3B	�LB	�dB	�wB	�wB	��B	��B	�}B	�wB	�jB	�dB	�^B	�dB	�jB	B	ÖB	ĜB	ĜB	ÖB	ƨB	��B	��B	��B	��B	��B	��B	�
B	�B	��B	��B	�
B	�B	�#B	�;B	�;B	�5B	�/B	�5B	�5B	�TB	�mB	�mB	�mB	�fB	�mB	�yB	�yB	�B	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
+B
+B
1B
1B
1B
	7B
JB
DB
	7B
	7B
JB
JB
JB
PB
VB
\B
bB
hB
bB
\B
\B
hB
hB
bB
bB
oB
uB
oB
uB
oB
oB
hB
bB
bB
oB
uB
uB
hB
oB
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
 �B
�B
#�B
%�B
%�B
$�B
$�B
%�B
%�B
&�B
&�B
&�B
'�B
'�B
'�B
(�B
)�B
)�B
+B
,B
,B
+B
+B
,B
,B
-B
-B
-B
0!B
0!B
0!B
/B
/B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
2-B
2-B
2-B
33B
33B
33B
33B
33B
49B
6FB
6FB
6FB
6FB
6FB
5?B
5?B
6FB
6FB
6FB
8RB
7LB
8RB
8RB
9XB
:^B
:^B
9XB
:^B
:^B
<jB
;dB
:^B
;dB
>wB
@�B
A�B
A�B
@�B
@�B
@�B
A�B
A�B
A�B
?}B
@�B
@�B
@�B
A�B
A�B
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
D�B
D�B
E�B
E�B
E�B
E�B
D�B
E�B
E�B
G�B
G�B
F�B
E�B
F�B
I�B
H�B
I�B
I�B
I�B
H�B
H�B
I�B
K�B
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
N�B
N�B
N�B
N�B
N�B
N�B
M�B
N�B
O�B
N�B
N�B
M�B
M�B
P�B
P�B
O�B
O�B
Q�B
Q�B
Q�B
P�B
P�B
O�B
Q�B
Q�B
Q�B
S�B
T�B
T�B
T�B
T�B
S�B
T�B
T�B
S�B
T�B
T�B
T�B
VB
VB
VB
VB
W
B
T�B
XB
XB
XB
XB
XB
XB
XB
XB
XB
XB
YB
XB
W
B
T�B
YB
YB
ZB
\)B
\)B
]/B
\)B
\)B
\)B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
`BB
`BB
_;B
^5B
_;B
`BB
_;B
^5B
_;B
aHB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
bNB
dZB
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
e`B
ffB
ffB
ffB
ffB
e`B
ffB
gmB
gmB
hsB
iyB
iyB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
hsB
iyB
jB
jB
jB
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
m�B
m�B
l�B
l�B
l�B
m�B
m�B
m�B
n�B
m�B
l�B
n�B
n�B
n�B
n�B
o�B
o�B
n�B
o�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�BhBhBhBhBhBhB�B�B�BhBhB�BhBhBhB}BvBpBjB
�B�B��B 4B-B;BUB�.B BB B B�B�B�B�>B��B�B�9B�-B�B��B��B��B��B��B��B��BѷB�pBȀB��B�|B��B�B|�BT�B<�BEB��B�	B�XB�vB{dBj�Be�Bp�Bo�Bk�BcTBJ#BS@BS@BN<BB�B2�B1�B%FB�B
�B
߾B
��B
�"B
��B
�dB
յB
бB
ƨB
��B
��B
��B
�B
v+B
h�B
l"B
mwB
]�B
I7B
#B
�B
!�B
�B	��B	�qB	��B	�B	�IB	�sB	��B	�
B	�dB	�AB	��B	��B	�`B	�kB	��B	��B	�HB	�;B	�WB	�{B	�<B	�B	�4B	z�B	n�B	]IB	9�B	'8B	;�B	N<B	LJB	J=B	CaB	<jB	72B	-wB	eB	[B	$�B	�B	!HB	IB	QB	�B	^B	B	B	�B		�B	
�B	�B�B��B��B��B�RB��B�B�B�>B��B��B��B��B�B�FB�rB�)B��B�9B�TB�xB��B�B�oB��B�DB��B��B�@B��B��B�jB�|B��B�]B�B�]B�7B��B��B��B�B�B�B�B�B��B{�BoOB`BB[�By�B{0B|B|6B{BzBwBs3Bp;Bc�B^�BfB[�BSuBe�Ba�B\�BW
BX�BU�BU�BMPBH�BSuBL�BG�BB�BGEB2�B2�BF%BAUB4B7�B>B>B;�B7B2aB1[B4B7�B5�B2�B3�B/�B-�B \B BB&�B.�B+�B+�B.cB+kB"�BB�B"�B!|B�B�B#�B%`B%zB%zB!BpBBBpB�B�B�B�wBABBeBxB�B]B"NB�BdB�BkB�B�B�B(>B(>B%�B-CB.IB/OB/OB/5B.IB+kB#�B�B!�B�BjB4�B:�B9�B:�B9�B8�B5B9�B<�B<�B;�B<�B9	B8B;0B<B@ B=<B:xBG�BJ	BJ	BIBG_BK)BIlBG�BOBBP}BM�BT{BU�B]IB]dB\�BYBY�BT,B\�Bi�Bf�Bh�Bf�BfBi�BiBhsBnIBtTB|6B�4B�;B�MB�aB�SB�SB�SB�mB�YB�gB�{B�YB�lB��B��B��B�pB�pB��B��B��B��B��B��B��B��B�B�1B�YB�OB�B�ZB��B��B��B�XB��B��B��B��B�OB��B��B��B�B�B��B�B�[B�SB�YB�eB�jB�B�nB�B�B�B��B��B��B��B��B��B��B��B�B��B��B��B��B�JB�HB	gB		�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	B	 B	# B	%B	$B	# B	!HB	*B	*KB	*eB	-]B	/OB	0;B	2GB	3hB	4TB	3�B	6zB	<�B	=�B	>�B	>�B	?�B	@�B	?�B	>�B	=B	C�B	E�B	J�B	K�B	MB	K�B	MB	L0B	NB	P.B	TFB	VmB	S�B	XyB	_pB	c�B	ezB	f�B	g�B	m�B	m�B	m�B	k�B	m�B	q�B	v�B	w2B	{B	~(B	B	�B	�4B	�UB	�-B	�9B	�YB	�EB	�YB	�SB	�SB	�KB	�rB	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�
B	��B	�
B	�
B	�B	�B	�*B	�*B	�/B	�AB	�aB	�hB	�fB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	B	ðB	ĶB	��B	��B	��B	��B	��B	��B	�B	�"B	�2B	�
B	�B	�aB	�aB	�sB	ևB	�qB	�pB	�VB	�jB	ݘB	ބB	ޞB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�B	�B	�B	�2B	�$B	��B	�B	�B	�B	�B	�B	�B	�(B	�(B	�(B	�.B
 4B
B
B
AB
'B
-B
B
aB
UB
MB
MB
+B
EB
fB
KB
�B
	RB
dB
^B
	lB
	lB
~B
dB
~B
jB
�B
�B
}B
hB
�B
�B
�B
hB
�B
�B
�B
�B
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
 �B
!B
 B
$B
%�B
%�B
%B
%B
&B
&B
'B
'B
'B
(
B
(
B
($B
)B
*0B
*B
+B
,"B
,"B
+6B
+QB
,"B
,"B
-)B
-)B
-]B
0!B
0!B
0;B
/5B
/5B
0UB
0UB
0;B
0UB
0UB
0;B
0UB
2aB
2aB
2GB
33B
3hB
3MB
3hB
3MB
4TB
6FB
6FB
6`B
6`B
6zB
5ZB
5ZB
6zB
6`B
6�B
8�B
7�B
8lB
8�B
9rB
:xB
:xB
9�B
:�B
:�B
<�B
;�B
:�B
;�B
>�B
@�B
A�B
A�B
@�B
@�B
@�B
A�B
A�B
A�B
?�B
@�B
@�B
@�B
A�B
A�B
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
D�B
D�B
E�B
E�B
E�B
E�B
D�B
E�B
E�B
G�B
G�B
F�B
E�B
F�B
I�B
H�B
I�B
I�B
I�B
H�B
H�B
I�B
K�B
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
N�B
N�B
N�B
N�B
N�B
OB
M�B
N�B
O�B
OB
N�B
NB
NB
Q B
P�B
O�B
PB
RB
Q�B
RB
Q B
QB
PB
RB
R B
R B
TB
T�B
UB
T�B
UB
TB
T�B
U2B
TB
UB
UB
UB
VB
VB
VB
VB
W
B
U2B
XB
XB
X+B
X+B
X+B
XEB
X+B
XEB
X+B
XEB
YKB
X+B
WYB
U�B
Y1B
YKB
ZQB
\)B
\CB
]/B
\CB
\]B
\CB
^5B
^OB
^5B
^OB
^jB
^OB
^jB
`BB
`\B
_pB
^jB
_VB
`BB
_pB
^jB
_pB
abB
bhB
cTB
cnB
cTB
cnB
cTB
cTB
cTB
cnB
b�B
dtB
ezB
ezB
dtB
dZB
ezB
d�B
dtB
dtB
dtB
d�B
ezB
ffB
f�B
f�B
f�B
ezB
f�B
g�B
g�B
hsB
iyB
iyB
hsB
hsB
h�B
i�B
iyB
iyB
i�B
i�B
i�B
iyB
i�B
i�B
h�B
i�B
j�B
j�B
j�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
m�B
m�B
l�B
l�B
l�B
m�B
m�B
m�B
n�B
m�B
l�B
n�B
n�B
n�B
n�B
o�B
o�B
n�B
o�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.07(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201802170034172018021700341720180217003417201806221237392018062212373920180622123739201804050434262018040504342620180405043426  JA  ARFMdecpA19c                                                                20180213093522  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180213003528  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180213003531  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180213003531  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180213003532  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180213003532  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180213003533  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180213003533  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180213003534  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180213003534                      G�O�G�O�G�O�                JA  ARUP                                                                        20180213005648                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180213153246  CV  JULD            G�O�G�O�F�_�                JM  ARCAJMQC2.0                                                                 20180216153417  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180216153417  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404193426  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622033739  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115121516                      G�O�G�O�G�O�                