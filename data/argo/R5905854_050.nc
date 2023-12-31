CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:53:30Z creation;2022-06-04T17:53:31Z conversion to V3.1      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8<   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8@   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8H   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8L   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8T   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8\   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8d   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8h   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8p   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8t   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8x   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8|   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9|   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tx   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �0   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �p   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �(   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ޘ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �    HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �D   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �T   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �X   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �h   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �l   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �p   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20220604175330  20220610141505  5905854                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               2A   JA                                  2B  A   APEX                            8422                            2.11.2                          846 @�"�W�1   @�" r�K@.�|�hs�c%�7Kƨ1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @���A   AffA@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�33B�ffB�ffB�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33Bę�B���B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�ffB���B���C   C  C  C  C  C
  C  C  C�C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4�C633C8  C:  C<  C>  C@  CB  CC�fCF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd�Cf�Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"�fD#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Du��Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�C3D��3D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D¼�D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܃3D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�` 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�@{�@��\@�AG�A>�HA^�HA~�HA�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�B�RB�RB�RB�RB'�RB/�RB7�RB?�RBG�RBO�RBW�RB_�RBg�RBo�RBw�RB�RB�\B�B�B�B�B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B�\B�u�BǨ�B��)B��)B��)B��)B��)B��)B��)B��)B�\B��)B�B�B�u�B���B��)C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C4�C6!GC7�C9�C;�C=�C?�CA�CC�zCE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cd�Cf�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
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
C��
C��
C��
C��
C��
C��=C��=C��=C��
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
D {�D ��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D	{�D	��D
{�D
��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D {�D ��D!{�D!��D"��D"��D#{�D#��D${�D$��D%{�D%��D&{�D&��D'{�D'��D({�D(��D){�D)��D*{�D*��D+{�D+��D,{�D,��D-{�D-��D.{�D.��D/{�D/��D0{�D0��D1{�D1��D2{�D2��D3{�D3��D4{�D4��D5{�D5��D6{�D6��D7{�D7��D8{�D8��D9{�D9��D:{�D:��D;{�D;��D<{�D<��D={�D=��D>{�D>��D?{�D?��D@{�D@��DA{�DA��DB{�DB��DC{�DC��DD{�DD��DE{�DE��DF{�DF��DG{�DG��DH{�DH��DI{�DI��DJ{�DJ��DK{�DK��DL{�DL��DM{�DM��DN{�DN��DO{�DO��DP{�DP��DQ{�DQ��DR{�DR��DS{�DS��DT{�DT��DU{�DU��DV{�DV��DW{�DW��DX{�DX��DY{�DY��DZ{�DZ��D[{�D[��D\{�D\��D]{�D]��D^{�D^��D_{�D_��D`{�D`��Da{�Da��Db{�Db��Dc{�Dc��Dd{�Dd��De{�De��Df{�Df��Dg{�Dg��Dh{�Dh��Di{�Di��Dj{�Dj��Dk{�Dk��Dl{�Dl��Dm{�Dm��Dn{�Dn��Do{�Do��Dp{�Dp��Dq{�Dq��Dr{�Dr��Ds{�Ds��Dt{�Dt��Du{�Du�Dv{�Dv��Dw{�Dw��Dx{�Dx��Dy{�Dy��Dz{�Dz��D{{�D{��D|{�D|��D}{�D}��D~{�D~��D{�D��D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D� �D�@�D���D���D���D�=�D���D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�@�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�Dº�D���D�=�D�}�Dý�D���D�=�D�}�DĽ�D���D�=�D�}�DŽ�D���D�=�D�}�Dƽ�D���D�=�D�}�Dǽ�D���D�=�D�}�DȽ�D���D�=�D�}�Dɽ�D���D�=�D�}�Dʽ�D���D�=�D�}�D˽�D���D�=�D�}�D̽�D���D�=�D�}�Dͽ�D���D�=�D�}�Dν�D���D�=�D�}�DϽ�D���D�=�D�}�Dн�D���D�=�D�}�Dѽ�D���D�=�D�}�Dҽ�D���D�=�D�}�Dӽ�D���D�=�D�}�DԽ�D���D�=�D�}�Dս�D���D�=�D�}�Dֽ�D���D�=�D�}�D׽�D���D�=�D�}�Dؽ�D���D�=�D�}�Dٽ�D���D�=�D�}�Dڽ�D���D�=�D�}�D۽�D���D�=�D܀�Dܽ�D���D�=�D�}�Dݽ�D���D�=�D�}�D޽�D���D�=�D�}�D߽�D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�z�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D���D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�]�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A̵A̴nA̯�A̪eA̶FA̹�A̻0A̼6A̽A̾wA̼�A̽�A���A̿}A��A���A��UA���A���A���A��?A��KA���A��A�ɆA�ʌA��0A���A��A�бA��HA��'A̫A�!�A˱[A�l�A�A��A�ĜA�9�A�A��A¦�A�IA�cTA�s�A�]�A�a�A�XyA��A� 'A�MA���A���A��A��bA��A�zxA�%�A��)A��A��PA��A��A���A�FA���A��mA��xA�gA���A�zDA�p�A��RA�Y�A��sA�{�A�K^A�f2A�uA���A���A���A�uA�A�hsA���A�Q�A���A�&�A�>�A��A���A�AxݘAp��Am�AhZAc��Aa1�A^ĜAZ�hAU��AP�AK��AG�IAEN�AD  AB��A?��A=��A<ϫA<K�A;ĜA:�A9��A8p;A7�qA7�A4��A2�IA2*�A1+kA/��A.��A-�<A-A+�rA*��A)p;A(GA'YKA&K�A$��A$}VA$G�A#��A"ϫA!�'AM�A�6A�FAc�A~A�KAjA�A��A+kAZ�A�gA�.A!-A�AA�A~�A�	A�A��A�A�AoA�A�>A��A��A�A˒A�fAr�A!�A�AAƨAaA��A��A��A�FAAVA��A~�ASA�Am]Ae�A
�}A
Z�A	��A	��A	K^A��AZA��AK�A�+A�eA'�A�KA�{AA�A��A�A��AffA�yAl�A&�A�AAɆAr�A=qA �A �A 	�@��	@���@�H@�ϫ@��f@��Y@�ݘ@��@�X�@�s�@�A @���@�ϫ@�c@�oi@�o�@��	@�,=@�0�@�V�@�t�@�4@���@�M�@��Q@��@�o@ꍹ@��.@��@�`�@��@�*�@�|@���@�1@�_p@�_�@�"@��@�ff@ߎ"@��f@�Xy@��@݌~@���@�u%@��@��
@�a@��?@��T@لM@�s@�Y@�V@��r@�c@��@ֿ�@�xl@�\�@Ցh@�7L@� \@���@԰!@�M@��@�@�F�@���@ґ�@�~�@�r�@�;�@Ѿw@��@���@Ёo@�خ@�t�@��c@���@Ω�@�Z@��o@�@̪e@̧�@˹�@�N<@�H�@��@�M@�	�@��Z@��9@�/@ȭ�@�S�@��@Ǚ�@�-w@��X@ƀ�@�&�@�P�@�E9@�;d@�-w@�"�@���@�Ta@��@ê�@�5�@�%@@�)�@���@���@�E9@��@���@��}@�_�@���@�Y�@��v@�˒@���@�U�@���@���@�R�@�b@���@�(�@�}V@�}V@�h�@�xl@�q@��@�A @���@�l�@� �@��3@��@�o@� i@��`@�Ĝ@���@�/�@�k�@��@��\@��H@��@�H@��-@�]�@��R@�@��@�r�@�M@�ݘ@���@��4@�A�@���@��4@�Ɇ@�i�@�B[@�!@���@���@��n@��$@���@�g�@�-w@�	l@�;@���@��@�� @�+k@�˒@��E@�&�@�@�U�@�Y@��}@��@��@�zx@��@��e@�M�@�-@��@�J�@�
=@���@�L0@��@��g@�Y@��}@���@���@���@�H�@�7�@� �@��r@��@�8�@��@�@��@�[�@��9@��-@��=@�x@�V@��E@��@�H@��j@���@�4@��@��@���@�y>@�)�@�ԕ@���@�W?@�=�@��@��@�Ft@��@��W@��q@��@�U�@�q@��@��@�bN@�]d@�H@�($@��@��}@��@�|�@�'�@���@���@�u�@�A�@�@�|�@�P�@�$t@��/@�tT@�A�@��@���@��Q@��@�Z�@���@���@�� @��@�W�@�G@��3@���@�{J@�]�@�'�@���@�S�@�e@�  @�خ@��}@��@���@�kQ@���@�dZ@�@@��K@���@�g8@�V@�GE@�<�@�*�@��q@�\)@��@��'@���@��@��T@�|�@�ߤ@��F@�W�@�"h@�	�@�خ@��h@�/@��@�O@��@��q@���@�2a@��@��@���@�7�@�F@9�@~��@~@}�#@}��@|��@|~(@|�@{�@{��@{��@z͟@z\�@y�N@x֡@x�@x-�@w��@w]�@w33@v6�@t��@tN�@t~@s�r@s��@s��@s��@s��@s�F@s�*@sx@s>�@r�"@r.�@q}�@q�@pK^@o�@n�@n��@n�F@n� @nZ�@m��@m�@m7L@l��@l�U@l֡@l��@lI�@k��@j�@i�>@i&�@h��@h�/@h�@hw�@h4n@gC@f8�@e�@e�~@ef�@ea�@eL�@e@@d�z@dH@cs@cH�@c4�@c�@b��@b;�@a�T@a�M@a0�@`�`@`�Y@_ݘ@_��@_�]@_�@_�{@_33@^�8@^�c@^ȴ@^� @^C�@]�j@]�S@]N<@];@\ѷ@\bN@\%�@[�&@[�:@[�@ZQ@ZO@ZJ@Z �@Y��@Y�@Y��@Yq@X�p@X$@W�{@W4�@V�@V��@VZ�@U�@T�@T��@Tj@T�@S��@S�K@S��@Sa@S@O@R�c@Rl�@RB[@R+k@R	@Q�)@Q��@Qw2@Q+�@P�5@P��@P�Y@P*�@O�@O�{@N�H@Nff@N-@M�'@Mw2@MT�@M@L�@L�@L��@LZ@L/�@LM@L@K�@K�g@K��@KZ�@J��@J�@J��@JW�@J�@I��@Ic@I%@H�o@H~@G�@GMj@G�@F�@F��@F\�@F	@F �@E�@E��@ES&@E�@D��@D>B@C��@C,�@C�@B͟@B^5@B	@A��@A��@A��@AG�@@�	@@��@@`�@@D�@?ݘ@?U�@?�@>҉@>��@>�@=��@=��@=��@=w2@<��@<�@<�4@<��@<Xy@;�W@;�P@;y�@;g�@;W?@;9�@:�M@:�}@:YK@:
�@9�@9|@9#�@8�f@8�@8��@8?�@8@7��@7��@7��@7A�@7S@6�'@6~�@6@�@5��@5V@4�[@4tT@4C-@3��@3��@3l�@3W?@3Y@2��@2_�@2J@1��@1w2@1<6@1�@0��@0��@0��@0c�@0,=@/��@/�P@/6z@.�X@.{�@-�@-��@-*0@,��@,Ft@,�@+�@+6z@+!-@*�c@*�,@*�X@*�}@*�@*��@)�@)�^@)p�@)O�@)�@(�)@(��@(��@(~(@(D�@(�@'��@'��@'H�@&��@&�X@&��@&h
@%��@%x�@%/@$�5@$��@$[�@#�m@#�q@#�:@#�@#qv@#/�@"��@"��@"Z�@!��@!�@!zx@!w2@!k�@!Dg@ �@ �D@�r@�@��@��@_�@�@�z@�~@a�@L�@F@�@��@`�@H@H@�	@33@/�@)_@!-@�@Y@�y@��@l�@ff@C�@{@�@ �@�.@ �@��@ϫ@��@f�@?}@�@�	@�5@��@�9@bN@7@�@�A@�F@x@]�@P�@>�@$t@�@�@q�@\�@L0@�@�@��@�H@��@j@�@�@�	@�@��@��@oi@bN@%�@�
@�V@��@ȴ@��@��@d�@0U@�@��@�-@Y�@��@�/@�$@z�@l"@7@��@�{@j�@K�@+@Y@@�"@�@�X@��@}V@5?@�.@�j@��@��@��@j@A @��@��@c�@1@�
@�@n/@_p@U�@8@
�s@
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A̵A̴nA̯�A̪eA̶FA̹�A̻0A̼6A̽A̾wA̼�A̽�A���A̿}A��A���A��UA���A���A���A��?A��KA���A��A�ɆA�ʌA��0A���A��A�бA��HA��'A̫A�!�A˱[A�l�A�A��A�ĜA�9�A�A��A¦�A�IA�cTA�s�A�]�A�a�A�XyA��A� 'A�MA���A���A��A��bA��A�zxA�%�A��)A��A��PA��A��A���A�FA���A��mA��xA�gA���A�zDA�p�A��RA�Y�A��sA�{�A�K^A�f2A�uA���A���A���A�uA�A�hsA���A�Q�A���A�&�A�>�A��A���A�AxݘAp��Am�AhZAc��Aa1�A^ĜAZ�hAU��AP�AK��AG�IAEN�AD  AB��A?��A=��A<ϫA<K�A;ĜA:�A9��A8p;A7�qA7�A4��A2�IA2*�A1+kA/��A.��A-�<A-A+�rA*��A)p;A(GA'YKA&K�A$��A$}VA$G�A#��A"ϫA!�'AM�A�6A�FAc�A~A�KAjA�A��A+kAZ�A�gA�.A!-A�AA�A~�A�	A�A��A�A�AoA�A�>A��A��A�A˒A�fAr�A!�A�AAƨAaA��A��A��A�FAAVA��A~�ASA�Am]Ae�A
�}A
Z�A	��A	��A	K^A��AZA��AK�A�+A�eA'�A�KA�{AA�A��A�A��AffA�yAl�A&�A�AAɆAr�A=qA �A �A 	�@��	@���@�H@�ϫ@��f@��Y@�ݘ@��@�X�@�s�@�A @���@�ϫ@�c@�oi@�o�@��	@�,=@�0�@�V�@�t�@�4@���@�M�@��Q@��@�o@ꍹ@��.@��@�`�@��@�*�@�|@���@�1@�_p@�_�@�"@��@�ff@ߎ"@��f@�Xy@��@݌~@���@�u%@��@��
@�a@��?@��T@لM@�s@�Y@�V@��r@�c@��@ֿ�@�xl@�\�@Ցh@�7L@� \@���@԰!@�M@��@�@�F�@���@ґ�@�~�@�r�@�;�@Ѿw@��@���@Ёo@�خ@�t�@��c@���@Ω�@�Z@��o@�@̪e@̧�@˹�@�N<@�H�@��@�M@�	�@��Z@��9@�/@ȭ�@�S�@��@Ǚ�@�-w@��X@ƀ�@�&�@�P�@�E9@�;d@�-w@�"�@���@�Ta@��@ê�@�5�@�%@@�)�@���@���@�E9@��@���@��}@�_�@���@�Y�@��v@�˒@���@�U�@���@���@�R�@�b@���@�(�@�}V@�}V@�h�@�xl@�q@��@�A @���@�l�@� �@��3@��@�o@� i@��`@�Ĝ@���@�/�@�k�@��@��\@��H@��@�H@��-@�]�@��R@�@��@�r�@�M@�ݘ@���@��4@�A�@���@��4@�Ɇ@�i�@�B[@�!@���@���@��n@��$@���@�g�@�-w@�	l@�;@���@��@�� @�+k@�˒@��E@�&�@�@�U�@�Y@��}@��@��@�zx@��@��e@�M�@�-@��@�J�@�
=@���@�L0@��@��g@�Y@��}@���@���@���@�H�@�7�@� �@��r@��@�8�@��@�@��@�[�@��9@��-@��=@�x@�V@��E@��@�H@��j@���@�4@��@��@���@�y>@�)�@�ԕ@���@�W?@�=�@��@��@�Ft@��@��W@��q@��@�U�@�q@��@��@�bN@�]d@�H@�($@��@��}@��@�|�@�'�@���@���@�u�@�A�@�@�|�@�P�@�$t@��/@�tT@�A�@��@���@��Q@��@�Z�@���@���@�� @��@�W�@�G@��3@���@�{J@�]�@�'�@���@�S�@�e@�  @�خ@��}@��@���@�kQ@���@�dZ@�@@��K@���@�g8@�V@�GE@�<�@�*�@��q@�\)@��@��'@���@��@��T@�|�@�ߤ@��F@�W�@�"h@�	�@�خ@��h@�/@��@�O@��@��q@���@�2a@��@��@���@�7�@�F@9�@~��@~@}�#@}��@|��@|~(@|�@{�@{��@{��@z͟@z\�@y�N@x֡@x�@x-�@w��@w]�@w33@v6�@t��@tN�@t~@s�r@s��@s��@s��@s��@s�F@s�*@sx@s>�@r�"@r.�@q}�@q�@pK^@o�@n�@n��@n�F@n� @nZ�@m��@m�@m7L@l��@l�U@l֡@l��@lI�@k��@j�@i�>@i&�@h��@h�/@h�@hw�@h4n@gC@f8�@e�@e�~@ef�@ea�@eL�@e@@d�z@dH@cs@cH�@c4�@c�@b��@b;�@a�T@a�M@a0�@`�`@`�Y@_ݘ@_��@_�]@_�@_�{@_33@^�8@^�c@^ȴ@^� @^C�@]�j@]�S@]N<@];@\ѷ@\bN@\%�@[�&@[�:@[�@ZQ@ZO@ZJ@Z �@Y��@Y�@Y��@Yq@X�p@X$@W�{@W4�@V�@V��@VZ�@U�@T�@T��@Tj@T�@S��@S�K@S��@Sa@S@O@R�c@Rl�@RB[@R+k@R	@Q�)@Q��@Qw2@Q+�@P�5@P��@P�Y@P*�@O�@O�{@N�H@Nff@N-@M�'@Mw2@MT�@M@L�@L�@L��@LZ@L/�@LM@L@K�@K�g@K��@KZ�@J��@J�@J��@JW�@J�@I��@Ic@I%@H�o@H~@G�@GMj@G�@F�@F��@F\�@F	@F �@E�@E��@ES&@E�@D��@D>B@C��@C,�@C�@B͟@B^5@B	@A��@A��@A��@AG�@@�	@@��@@`�@@D�@?ݘ@?U�@?�@>҉@>��@>�@=��@=��@=��@=w2@<��@<�@<�4@<��@<Xy@;�W@;�P@;y�@;g�@;W?@;9�@:�M@:�}@:YK@:
�@9�@9|@9#�@8�f@8�@8��@8?�@8@7��@7��@7��@7A�@7S@6�'@6~�@6@�@5��@5V@4�[@4tT@4C-@3��@3��@3l�@3W?@3Y@2��@2_�@2J@1��@1w2@1<6@1�@0��@0��@0��@0c�@0,=@/��@/�P@/6z@.�X@.{�@-�@-��@-*0@,��@,Ft@,�@+�@+6z@+!-@*�c@*�,@*�X@*�}@*�@*��@)�@)�^@)p�@)O�@)�@(�)@(��@(��@(~(@(D�@(�@'��@'��@'H�@&��@&�X@&��@&h
@%��@%x�@%/@$�5@$��@$[�@#�m@#�q@#�:@#�@#qv@#/�@"��@"��@"Z�@!��@!�@!zx@!w2@!k�@!Dg@ �@ �D@�r@�@��@��@_�@�@�z@�~@a�@L�@F@�@��@`�@H@H@�	@33@/�@)_@!-@�@Y@�y@��@l�@ff@C�@{@�@ �@�.@ �@��@ϫ@��@f�@?}@�@�	@�5@��@�9@bN@7@�@�A@�F@x@]�@P�@>�@$t@�@�@q�@\�@L0@�@�@��@�H@��@j@�@�@�	@�@��@��@oi@bN@%�@�
@�V@��@ȴ@��@��@d�@0U@�@��@�-@Y�@��@�/@�$@z�@l"@7@��@�{@j�@K�@+@Y@@�"@�@�X@��@}V@5?@�.@�j@��@��@��@j@A @��@��@c�@1@�
@�@n/@_p@U�@8@
�s@
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	=VB	=�B	=�B	=�B	=<B	=�B	=qB	=�B	=qB	=VB	=qB	=�B	=�B	=qB	=qB	=VB	=qB	=VB	=VB	=qB	=qB	=qB	=�B	=qB	=�B	=�B	=�B	=�B	=�B	=�B	=VB	=qB	=�B	72B	3�B	�B		B	)B	NB	�B	;0B	_�B	z�B	��B	��B	��B	��B
'8B
'mB
.�B
CaB
W�B
bNB
�B
��B
�rB
�{B
��B?.BN�BS[Br|Br�BpUBpoBp�B\�BYBO\B4�B'�B(sB*�B�VB�B��B�sB��B��BkkB[�BG+B,WBeB
��B
�B
�fB
��B
��B
`�B
O�B
G�B
/5B	�xB	��B	�@B	u?B	YKB	D�B	;�B	0�B	B	
�B��B�UB�B�B׍B�uB�TB�
BڠBܬB��B�B�B�B�JB	�B	OB	&2B	&B	*�B	1�B	2�B	5tB	<�B	J�B	MPB	P�B	U�B	Z�B	XEB	R�B	PHB	O\B	QNB	T�B	PB	B�B	,�B	$�B	'�B	)�B	HKB	m)B	z*B	~B	��B	�B	��B	�BB	�BB	�4B	�$B	��B	�CB	�#B	��B	��B	��B	��B	�;B	��B	�(B	��B	��B	��B	�WB	��B	̳B	�<B	�B	�@B	�MB	��B	�&B	� B	�oB	�FB	�mB	��B	՛B	�B	�YB	��B	�B	�B	�7B	�]B	��B	ݘB	�B	�B	�]B	�dB	�;B	�VB	��B	߾B	�'B	��B	�|B	��B	��B	�B	��B	ߤB	��B	ߊB	�;B	��B	��B	�B	�jB	ݘB	�IB	�jB	�B	��B	�B	�hB	�4B	�B	��B	�bB	�B	�HB	��B	߾B	�pB	ބB	��B	ܒB	�xB	�WB	�B	�B	�
B	�B	�B	�B	ٴB	�	B	ںB	�7B	ؓB	�B	��B	׍B	��B	�B	�KB	�+B	�1B	��B	޸B	�B	�IB	�B	��B	��B	��B	�B	߾B	ߊB	�;B	�VB	��B	�B	��B	�vB	��B	��B	��B	��B	�B	��B	��B	��B	�TB	�:B	�:B	�B	� B	�B	�B	�zB	�`B	��B	��B	��B	��B	�B	�FB	�,B	�@B	�zB	��B	�`B	��B	��B	�B	�B	��B	�B	��B	�B	�B	�sB	��B	�B	��B	�B	�B	�>B	�B	�B	�B	��B	�B	��B	�B	�yB	�B	�B	�6B	�)B	��B	�B	�GB	�B	�B	�hB	�B	�hB	��B	�3B	�hB	�9B	�?B	��B	�zB	�8B	��B	�XB	�	B	�DB	��B	�$B	��B	�lB	��B	��B	�RB	��B	�	B	��B	�jB	�B	�}B
;B
 4B	�wB	�]B	��B	��B
�B
 �B
�B
GB
-B
GB
GB
�B
�B
�B
�B
GB
�B	��B	��B	��B	��B	�JB	��B	��B	�JB	�B	�B	��B	��B	��B	��B	�]B	��B	��B	��B	�BB	�.B	��B
 �B
 �B
 �B
 B
 B
�B
�B
�B
MB
�B
�B
�B
�B
�B
GB
�B
'B
�B
�B
 B
 �B
 �B
B
;B
UB
B
-B
�B
�B
uB
B
[B
[B
gB
�B
YB
B
_B
zB
�B
B
�B
�B
�B
	B
	�B

�B

�B

�B

�B
^B
^B
�B
�B
0B
JB
�B
�B
�B
B
PB
B
�B
�B
(B
(B
�B
hB
 B
�B
hB
NB
�B
B
NB
�B
:B
 B
TB
�B
@B
uB
�B
uB
�B
�B
�B
�B
2B
�B
�B
�B
�B
�B
EB
�B
B
�B
KB
�B
B
=B
#B
WB
qB
qB
)B
xB
B
�B
B
OB
�B
�B
�B
;B
;B
VB
�B
;B
 �B
!HB
"hB
"�B
#B
#B
#�B
#�B
$B
$&B
$&B
$&B
%,B
%FB
%FB
%,B
%zB
&fB
&�B
'RB
'�B
(>B
(>B
(sB
(sB
(�B
(�B
)B
)�B
*KB
*eB
*�B
*KB
*�B
,"B
,WB
,�B
-CB
-]B
-B
,�B
,qB
,�B
.B
-�B
/5B
/OB
/5B
/5B
/OB
/�B
0B
0�B
0B
0UB
0�B
1[B
1vB
0�B
/�B
.�B
.�B
/5B
/�B
0!B
0�B
0�B
0�B
0�B
1AB
1'B
1�B
1�B
1AB
0�B
0oB
/�B
/OB
/B
/5B
/iB
0UB
1AB
1AB
1[B
0�B
1[B
2�B
3�B
3�B
2�B
3hB
3�B
3�B
3�B
3�B
4B
4B
49B
49B
5�B
5�B
6FB
6FB
6`B
6`B
6�B
7B
7�B
7�B
8�B
8�B
8�B
8�B
8�B
8�B
9$B
9�B
:^B
:xB
;B
:�B
;dB
;�B
<6B
<6B
<PB
<6B
<6B
<PB
<6B
<PB
<B
<PB
<jB
<�B
<�B
=�B
=�B
=�B
=<B
>BB
?B
>�B
>�B
>�B
>�B
>�B
?.B
?�B
?�B
@4B
@�B
@�B
A B
A;B
AoB
BB
B�B
B�B
B�B
CB
CGB
CGB
CaB
C{B
C�B
C�B
DMB
D�B
D�B
D�B
D�B
D�B
D�B
D�B
EB
ESB
EmB
E�B
E�B
E�B
FtB
F�B
G+B
G�B
G�B
G�B
HKB
HKB
HfB
H�B
IB
I7B
I7B
I7B
IRB
IRB
IlB
I�B
J=B
JXB
JXB
J�B
J�B
J�B
K)B
K�B
K�B
LJB
L�B
L�B
MB
MB
MPB
M�B
M�B
NB
M�B
N"B
NpB
N�B
OBB
OBB
O�B
P.B
PB
P.B
P}B
P�B
P�B
P�B
P�B
Q B
QNB
Q�B
Q�B
Q�B
R B
S&B
S@B
S&B
S�B
T{B
T�B
T�B
T�B
T�B
U2B
T�B
T�B
UB
U�B
U�B
V9B
V9B
V9B
V9B
V9B
V�B
V�B
W$B
W?B
WsB
W�B
XB
X+B
XB
XEB
X�B
X�B
YB
YB
YB
YB
Y�B
Y�B
Y�B
ZB
Z�B
Z�B
Z�B
[=B
[=B
[�B
[�B
[�B
[�B
\)B
\]B
\�B
\�B
]/B
]/B
]dB
]~B
]�B
^B
^B
^jB
^�B
^�B
_B
_VB
_�B
_�B
`'B
`BB
`�B
aHB
abB
a�B
a�B
bNB
bNB
b�B
b�B
b�B
cB
cB
cB
c�B
c�B
d@B
d@B
d�B
d�B
d�B
d�B
eB
e`B
e�B
e�B
e�B
e�B
f2B
fLB
f�B
f�B
g8B
gmB
gRB
g�B
g�B
h
B
h$B
hsB
h�B
h�B
iDB
j�B
j�B
k6B
kQB
j�B
k�B
k�B
k�B
l"B
l"B
lWB
l�B
m�B
m�B
m�B
m�B
mwB
mCB
mCB
m�B
m�B
m�B
m�B
m�B
n�B
oOB
oiB
o�B
p�B
q[B
q[B
q�B
q�B
q�B
q�B
rB
raB
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
sB
shB
s�B
s�B
s�B
s�B
s�B
tB
tB
tnB
t�B
t�B
t�B
uB
u?B
u?B
uZB
uZB
u�B
utB
u�B
v+B
vB
v+B
v`B
v`B
v�B
v�B
v�B
v�B
wLB
wLB
wfB
wfB
wfB
w�B
w�B
w�B
xB
xB
xB
x�B
x�B
yXB
yrB
yrB
y�B
y�B
y�B
zB
zDB
z�B
z�B
z�B
z�B
z�B
{B
{dB
{dB
{B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
|B
|PB
|PB
|�B
|�B
|�B
|�B
|�B
}<B
}qB
}�B
}�B
}�B
~(B
~�B
~�B
~�B
~�B
.B
c1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	=VB	=�B	=�B	=�B	=<B	=�B	=qB	=�B	=qB	=VB	=qB	=�B	=�B	=qB	=qB	=VB	=qB	=VB	=VB	=qB	=qB	=qB	=�B	=qB	=�B	=�B	=�B	=�B	=�B	=�B	=VB	=qB	=�B	72B	3�B	�B		B	)B	NB	�B	;0B	_�B	z�B	��B	��B	��B	��B
'8B
'mB
.�B
CaB
W�B
bNB
�B
��B
�rB
�{B
��B?.BN�BS[Br|Br�BpUBpoBp�B\�BYBO\B4�B'�B(sB*�B�VB�B��B�sB��B��BkkB[�BG+B,WBeB
��B
�B
�fB
��B
��B
`�B
O�B
G�B
/5B	�xB	��B	�@B	u?B	YKB	D�B	;�B	0�B	B	
�B��B�UB�B�B׍B�uB�TB�
BڠBܬB��B�B�B�B�JB	�B	OB	&2B	&B	*�B	1�B	2�B	5tB	<�B	J�B	MPB	P�B	U�B	Z�B	XEB	R�B	PHB	O\B	QNB	T�B	PB	B�B	,�B	$�B	'�B	)�B	HKB	m)B	z*B	~B	��B	�B	��B	�BB	�BB	�4B	�$B	��B	�CB	�#B	��B	��B	��B	��B	�;B	��B	�(B	��B	��B	��B	�WB	��B	̳B	�<B	�B	�@B	�MB	��B	�&B	� B	�oB	�FB	�mB	��B	՛B	�B	�YB	��B	�B	�B	�7B	�]B	��B	ݘB	�B	�B	�]B	�dB	�;B	�VB	��B	߾B	�'B	��B	�|B	��B	��B	�B	��B	ߤB	��B	ߊB	�;B	��B	��B	�B	�jB	ݘB	�IB	�jB	�B	��B	�B	�hB	�4B	�B	��B	�bB	�B	�HB	��B	߾B	�pB	ބB	��B	ܒB	�xB	�WB	�B	�B	�
B	�B	�B	�B	ٴB	�	B	ںB	�7B	ؓB	�B	��B	׍B	��B	�B	�KB	�+B	�1B	��B	޸B	�B	�IB	�B	��B	��B	��B	�B	߾B	ߊB	�;B	�VB	��B	�B	��B	�vB	��B	��B	��B	��B	�B	��B	��B	��B	�TB	�:B	�:B	�B	� B	�B	�B	�zB	�`B	��B	��B	��B	��B	�B	�FB	�,B	�@B	�zB	��B	�`B	��B	��B	�B	�B	��B	�B	��B	�B	�B	�sB	��B	�B	��B	�B	�B	�>B	�B	�B	�B	��B	�B	��B	�B	�yB	�B	�B	�6B	�)B	��B	�B	�GB	�B	�B	�hB	�B	�hB	��B	�3B	�hB	�9B	�?B	��B	�zB	�8B	��B	�XB	�	B	�DB	��B	�$B	��B	�lB	��B	��B	�RB	��B	�	B	��B	�jB	�B	�}B
;B
 4B	�wB	�]B	��B	��B
�B
 �B
�B
GB
-B
GB
GB
�B
�B
�B
�B
GB
�B	��B	��B	��B	��B	�JB	��B	��B	�JB	�B	�B	��B	��B	��B	��B	�]B	��B	��B	��B	�BB	�.B	��B
 �B
 �B
 �B
 B
 B
�B
�B
�B
MB
�B
�B
�B
�B
�B
GB
�B
'B
�B
�B
 B
 �B
 �B
B
;B
UB
B
-B
�B
�B
uB
B
[B
[B
gB
�B
YB
B
_B
zB
�B
B
�B
�B
�B
	B
	�B

�B

�B

�B

�B
^B
^B
�B
�B
0B
JB
�B
�B
�B
B
PB
B
�B
�B
(B
(B
�B
hB
 B
�B
hB
NB
�B
B
NB
�B
:B
 B
TB
�B
@B
uB
�B
uB
�B
�B
�B
�B
2B
�B
�B
�B
�B
�B
EB
�B
B
�B
KB
�B
B
=B
#B
WB
qB
qB
)B
xB
B
�B
B
OB
�B
�B
�B
;B
;B
VB
�B
;B
 �B
!HB
"hB
"�B
#B
#B
#�B
#�B
$B
$&B
$&B
$&B
%,B
%FB
%FB
%,B
%zB
&fB
&�B
'RB
'�B
(>B
(>B
(sB
(sB
(�B
(�B
)B
)�B
*KB
*eB
*�B
*KB
*�B
,"B
,WB
,�B
-CB
-]B
-B
,�B
,qB
,�B
.B
-�B
/5B
/OB
/5B
/5B
/OB
/�B
0B
0�B
0B
0UB
0�B
1[B
1vB
0�B
/�B
.�B
.�B
/5B
/�B
0!B
0�B
0�B
0�B
0�B
1AB
1'B
1�B
1�B
1AB
0�B
0oB
/�B
/OB
/B
/5B
/iB
0UB
1AB
1AB
1[B
0�B
1[B
2�B
3�B
3�B
2�B
3hB
3�B
3�B
3�B
3�B
4B
4B
49B
49B
5�B
5�B
6FB
6FB
6`B
6`B
6�B
7B
7�B
7�B
8�B
8�B
8�B
8�B
8�B
8�B
9$B
9�B
:^B
:xB
;B
:�B
;dB
;�B
<6B
<6B
<PB
<6B
<6B
<PB
<6B
<PB
<B
<PB
<jB
<�B
<�B
=�B
=�B
=�B
=<B
>BB
?B
>�B
>�B
>�B
>�B
>�B
?.B
?�B
?�B
@4B
@�B
@�B
A B
A;B
AoB
BB
B�B
B�B
B�B
CB
CGB
CGB
CaB
C{B
C�B
C�B
DMB
D�B
D�B
D�B
D�B
D�B
D�B
D�B
EB
ESB
EmB
E�B
E�B
E�B
FtB
F�B
G+B
G�B
G�B
G�B
HKB
HKB
HfB
H�B
IB
I7B
I7B
I7B
IRB
IRB
IlB
I�B
J=B
JXB
JXB
J�B
J�B
J�B
K)B
K�B
K�B
LJB
L�B
L�B
MB
MB
MPB
M�B
M�B
NB
M�B
N"B
NpB
N�B
OBB
OBB
O�B
P.B
PB
P.B
P}B
P�B
P�B
P�B
P�B
Q B
QNB
Q�B
Q�B
Q�B
R B
S&B
S@B
S&B
S�B
T{B
T�B
T�B
T�B
T�B
U2B
T�B
T�B
UB
U�B
U�B
V9B
V9B
V9B
V9B
V9B
V�B
V�B
W$B
W?B
WsB
W�B
XB
X+B
XB
XEB
X�B
X�B
YB
YB
YB
YB
Y�B
Y�B
Y�B
ZB
Z�B
Z�B
Z�B
[=B
[=B
[�B
[�B
[�B
[�B
\)B
\]B
\�B
\�B
]/B
]/B
]dB
]~B
]�B
^B
^B
^jB
^�B
^�B
_B
_VB
_�B
_�B
`'B
`BB
`�B
aHB
abB
a�B
a�B
bNB
bNB
b�B
b�B
b�B
cB
cB
cB
c�B
c�B
d@B
d@B
d�B
d�B
d�B
d�B
eB
e`B
e�B
e�B
e�B
e�B
f2B
fLB
f�B
f�B
g8B
gmB
gRB
g�B
g�B
h
B
h$B
hsB
h�B
h�B
iDB
j�B
j�B
k6B
kQB
j�B
k�B
k�B
k�B
l"B
l"B
lWB
l�B
m�B
m�B
m�B
m�B
mwB
mCB
mCB
m�B
m�B
m�B
m�B
m�B
n�B
oOB
oiB
o�B
p�B
q[B
q[B
q�B
q�B
q�B
q�B
rB
raB
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
sB
shB
s�B
s�B
s�B
s�B
s�B
tB
tB
tnB
t�B
t�B
t�B
uB
u?B
u?B
uZB
uZB
u�B
utB
u�B
v+B
vB
v+B
v`B
v`B
v�B
v�B
v�B
v�B
wLB
wLB
wfB
wfB
wfB
w�B
w�B
w�B
xB
xB
xB
x�B
x�B
yXB
yrB
yrB
y�B
y�B
y�B
zB
zDB
z�B
z�B
z�B
z�B
z�B
{B
{dB
{dB
{B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
|B
|PB
|PB
|�B
|�B
|�B
|�B
|�B
}<B
}qB
}�B
}�B
}�B
~(B
~�B
~�B
~�B
~�B
.B
c1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104955  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604175330  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604175331  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604175331                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605025338  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605025338  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610141505                      G�O�G�O�G�O�                