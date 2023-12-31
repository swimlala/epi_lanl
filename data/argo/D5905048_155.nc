CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-09-01T03:35:08Z creation;2017-09-01T03:35:12Z conversion to V3.1;2019-12-19T07:58:39Z update;     
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
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \|   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `\   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �<   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �|   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �\   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �\   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �l   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �p   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �t   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �x   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �|   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20170901033508  20200116221516  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0577_155                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�"�Er�1   @�"�    @42��ᰊ�d�i�B��1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cy�fC|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�0 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�(�@�@�A�HA>�HA^�HA~�HA�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�B�RB�RB�RB�RB'�RB/�RB7�RB?�RBG�RBO�RBW�RB_�RBg�RBo�RBw�RB�RB��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B�\B��)B��)B��)B��)B��)C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�{C{�C}�C�C��
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
C��
C��
C��
C��
C��
C��
C��
C��
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
��D{�D��D{�D��D{�D��D{�D��D{�D�D{�D��D{�D�D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D {�D ��D!{�D!��D"{�D"��D#{�D#��D${�D$��D%{�D%��D&{�D&��D'{�D'��D({�D(��D){�D)��D*{�D*��D+{�D+��D,{�D,��D-{�D-��D.{�D.��D/{�D/��D0{�D0��D1{�D1��D2{�D2��D3{�D3��D4{�D4��D5{�D5��D6{�D6��D7{�D7��D8{�D8��D9{�D9��D:{�D:��D;{�D;��D<{�D<��D={�D=��D>{�D>��D?{�D?��D@{�D@��DA{�DA��DB{�DB��DC{�DC��DD{�DD��DE{�DE��DF{�DF��DG{�DG��DH{�DH��DI{�DI��DJ{�DJ��DK{�DK��DL{�DL��DM{�DM��DN{�DN��DO{�DO��DP{�DP��DQ{�DQ��DR{�DR��DS{�DS��DT{�DT��DU{�DU��DV{�DV��DW{�DW��DX{�DX��DY{�DY��DZ{�DZ��D[{�D[��D\{�D\��D]{�D]��D^{�D^��D_{�D_��D`{�D`��Da{�Da��Db{�Db��Dc{�Dc��Dd{�Dd��De{�De��Df{�Df��Dg{�Dg��Dh{�Dh��Di{�Di��Dj{�Dj��Dk{�Dk��Dl{�Dl��Dm{�Dm��Dn{�Dn��Do{�Do��Dp{�Dp��Dq{�Dq��Dr{�Dr��Ds{�Ds��Dt{�Dt��Du{�Du��Dv{�Dv��Dw{�Dw��Dx{�Dx��Dy{�Dy��Dz{�Dz��D{{�D{��D|{�D|��D}{�D}��D~{�D~��D{�D��D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D� �D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D½�D���D�=�D�}�Dý�D���D�=�D�}�DĽ�D���D�=�D�}�DŽ�D���D�=�D�}�Dƽ�D���D�=�D�}�Dǽ�D���D�=�D�}�DȽ�D���D�=�D�}�Dɽ�D���D�=�D�}�Dʽ�D���D�=�D�}�D˽�D���D�=�D�}�D̽�D���D�=�D�}�Dͽ�D���D�=�D�}�Dν�D���D�=�D�}�DϽ�D���D�=�D�}�Dн�D���D�=�D�}�Dѽ�D���D�=�D�}�Dҽ�D���D�=�D�}�Dӽ�D���D�=�D�}�DԽ�D���D�=�D�}�Dս�D���D�=�D�}�Dֽ�D���D�=�D�}�D׽�D���D�=�D�}�Dؽ�D���D�=�D�}�Dٽ�D���D�=�D�}�Dڽ�D���D�=�D�}�D۽�D���D�=�D�}�Dܽ�D���D�=�D�}�Dݽ�D���D�=�D�}�D޽�D���D�=�D�}�D߽�D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D��D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D���D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D� �D�-�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�C�A�1'A��A�{A�oA�JA�A�  A���A���A���A���A���A���A���A���A���A��A��
Aܺ^A��mA�S�A��/AԴ9A�n�Aҝ�AоwA�33AϺ^A��;A�(�A;wA͋DA�(�A�XAʬA�v�A�A�  A�\)A�ƨA�5?A�v�A�33A��A���A��yA��A�r�A���A�ZA�A�"�A���A�?}A���A�|�A��A��uA��`A�\)A�ƨA��#A�7LA��;A��FA�`BA���A��mA��A���A��A��A�I�A��7A�9XA��A� �A��9A��9A��A�x�A��wA�E�A�A�A��yA�XA�ffA��A�?}A���A��A��RA���A�;dA��
A��A�;dA�-A�
=A���A�%A�r�A�A���A���A�A�ƨA�z�A�?}A�bNA�VA���A�^Az�!Aw��Av�!Avn�Au��AtM�Ar1'Ap��Ao�TAm��AlE�AkVAi;dAf=qAb�!Aa��Aa"�A_K�A\~�A[�PAY�AW��AS��AP��AO�PANA�ALJAJ�\AIAG;dAC&�A@��A@{A=��A:�+A9�FA9S�A8��A7��A6��A5XA3��A3VA2�A/�7A.��A.5?A,�yA+�wA+�A*I�A)dZA(jA'VA%33A$�HA#�;A#oA"�A �yAC�A��AQ�At�Ax�A��A  A�yA1A�;A|�AXAAl�A�RA�^A��A�A��A�A`BA�\A��AoAQ�Al�A
ZA	�A��A��AG�AĜAr�AbAVA�A�DA1A�uAM�A��AhsA/A ��@���@��@��@��!@��@��#@�G�@��P@��T@�9X@�ƨ@�@�1@�@�33@�hs@��@��@���@�A�@�n�@�J@��@�@�r�@��@܃@�~�@��@�  @��
@׶F@�b@��y@Չ7@�(�@�5?@��#@���@ѩ�@�G�@�Z@ύP@�^5@�-@�l�@�C�@ͺ^@�  @��
@�J@�Z@�=q@þw@Å@Õ�@å�@�S�@�V@�@�x�@��9@�"�@��R@�=q@���@��D@�1'@�9X@�  @�
=@���@��`@��@�b@��@���@���@�S�@���@��@���@���@���@��T@��@��u@�  @��
@���@�S�@��R@�5?@��@���@��9@��@�9X@��;@�t�@�ȴ@�M�@��T@�@��^@���@�hs@�?}@�%@��/@��/@�Ĝ@�Ĝ@�z�@�1@��
@��@���@�t�@�@�n�@��@�@���@��7@�O�@��@��@��w@��@���@�S�@�;d@�+@��H@�E�@�$�@���@���@���@�O�@���@���@��@���@��@�9X@�1@��
@��w@��P@�C�@�"�@�@��R@�^5@��@��@��^@���@�`B@�%@��/@��9@�bN@��@���@��m@��@�S�@��@�V@�=q@�$�@�{@��@��h@�p�@�X@��@��@�r�@�Q�@�1@��F@�t�@�dZ@�"�@��H@���@�v�@�5?@���@���@�O�@���@���@�j@�A�@�t�@�@��y@��@���@���@���@�~�@�ff@�=q@�J@��@��@�bN@�(�@��;@�
=@��@��R@�v�@�@�@���@��@�G�@��/@� �@�  @���@�t�@�+@��y@��H@���@��+@�5?@���@��T@�@�x�@�O�@�&�@��u@�I�@�9X@�1@���@�dZ@�K�@�
=@���@���@��!@�~�@�-@�J@�@�@�G�@���@�I�@�ƨ@��@�t�@�\)@�C�@�"�@�@��y@�ȴ@���@�{@��T@���@���@��@�V@�M�@�@���@�O�@���@��`@��/@���@�Ĝ@��9@��u@�j@�I�@�1'@�  @��m@��
@��w@��@�+@��R@���@�V@�$�@��T@���@��h@��7@�x�@�G�@���@���@��j@��@��D@�r�@��@K�@
=@~�y@~�+@~{@}�-@}`B@}V@|�@|(�@{�
@{��@{t�@z�@zn�@z�@zJ@y�^@y�7@y7L@x��@x��@x�9@x1'@x  @w�w@w\)@wK�@v��@u�T@u/@t�@t�@tz�@s"�@r~�@rn�@r^5@q��@q�7@p�`@pb@o��@oK�@o�@o
=@o
=@nE�@m�T@l�@lj@l(�@k��@k�m@k33@j~�@j^5@i��@i�7@i�@i%@h��@h�u@hA�@hb@g�w@f�y@f��@fff@fE�@fE�@f5?@f{@e��@e`B@e?}@d��@d��@d�j@c��@c�@cdZ@cC�@b��@a��@a&�@`��@`�u@` �@_��@_l�@^�R@^5?@]��@]`B@]`B@]O�@]�@\�@\9X@[��@[S�@Z�H@Zn�@Z^5@Z=q@Z�@Y�#@Y��@YX@Y7L@X�`@Xr�@XQ�@XA�@XA�@W��@Wl�@WK�@W+@Vȴ@U��@Up�@Up�@T�/@Tz�@T(�@S�F@SdZ@SS�@R��@R�\@Q��@Q��@Q�@PĜ@P�9@PQ�@O��@O\)@O
=@Nȴ@N�+@NV@N@Mp�@L��@Lz�@K�m@K��@K��@Kt�@K33@J��@J^5@I�@I��@IG�@I%@HĜ@H��@H�u@H�@Hr�@HA�@G�@G�w@Gl�@G;d@G
=@F�y@Fȴ@Fv�@FE�@E�T@E�-@E�@EO�@D�/@D�D@DZ@D(�@Cƨ@C��@C�@CdZ@C"�@B�H@B��@Bn�@BM�@BJ@A�@A��@@��@@�@?�;@?|�@?+@>ȴ@>��@>�+@=�T@=��@=�@=�@<�@<Z@<(�@<�@<1@;�m@;��@;�@;�@;dZ@;"�@:�H@:~�@:-@9�^@9��@9hs@9�@8��@8�9@8�9@8Q�@8b@7�P@7�@7
=@6�y@6��@6v�@5@5O�@4��@4�/@4�j@4�@4�D@4z�@4j@4(�@3��@3�
@3��@3"�@2�H@2�!@2~�@2n�@2M�@2=q@2�@1�#@1��@1��@1�^@1��@1G�@1&�@1&�@1�@1%@0��@0�u@0�@0b@/��@/K�@/+@/�@.�y@.�R@.��@.��@.@-�h@-`B@-�@,��@,�@,�D@,(�@+��@+��@+dZ@+C�@+33@+@*�@*�H@*n�@*^5@*M�@*=q@)��@)�#@)��@)��@)��@)��@)x�@)X@)�@(��@(��@(Ĝ@(��@(�u@(�@(Q�@(Q�@(1'@'�;@'��@'\)@';d@'
=@&�@&��@&ff@&@%�-@%�-@%�h@%p�@%?}@%�@$��@$�@$�/@$�@$j@$I�@$�@$1@$1@#�
@#�
@#�F@#�@#C�@"�@"�\@"M�@"-@"�@"J@!�@!�^@!x�@!G�@ �`@ ��@ Ĝ@ �9@ ��@ ��@ �u@ r�@ Q�@�@��@l�@+@�y@��@v�@E�@{@@�@��@�-@�h@O�@V@�/@z�@Z@(�@��@�
@�F@��@C�@33@"�@o@�@��@�!@n�@J@��@��@�@�#@��@hs@7L@&�@%@��@��@�9@r�@b@  @�@��@�P@
=@��@�y@�@�R@��@v�@E�@E�@$�@�T@��@@��@��@p�@/@��@��@�/@��@��@��@�j@��@Z@(�@�@��@�
@�F@��@��@��@�@S�@o@��@��@^5@�@�@��@�7@x�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�C�A�1'A��A�{A�oA�JA�A�  A���A���A���A���A���A���A���A���A���A��A��
Aܺ^A��mA�S�A��/AԴ9A�n�Aҝ�AоwA�33AϺ^A��;A�(�A;wA͋DA�(�A�XAʬA�v�A�A�  A�\)A�ƨA�5?A�v�A�33A��A���A��yA��A�r�A���A�ZA�A�"�A���A�?}A���A�|�A��A��uA��`A�\)A�ƨA��#A�7LA��;A��FA�`BA���A��mA��A���A��A��A�I�A��7A�9XA��A� �A��9A��9A��A�x�A��wA�E�A�A�A��yA�XA�ffA��A�?}A���A��A��RA���A�;dA��
A��A�;dA�-A�
=A���A�%A�r�A�A���A���A�A�ƨA�z�A�?}A�bNA�VA���A�^Az�!Aw��Av�!Avn�Au��AtM�Ar1'Ap��Ao�TAm��AlE�AkVAi;dAf=qAb�!Aa��Aa"�A_K�A\~�A[�PAY�AW��AS��AP��AO�PANA�ALJAJ�\AIAG;dAC&�A@��A@{A=��A:�+A9�FA9S�A8��A7��A6��A5XA3��A3VA2�A/�7A.��A.5?A,�yA+�wA+�A*I�A)dZA(jA'VA%33A$�HA#�;A#oA"�A �yAC�A��AQ�At�Ax�A��A  A�yA1A�;A|�AXAAl�A�RA�^A��A�A��A�A`BA�\A��AoAQ�Al�A
ZA	�A��A��AG�AĜAr�AbAVA�A�DA1A�uAM�A��AhsA/A ��@���@��@��@��!@��@��#@�G�@��P@��T@�9X@�ƨ@�@�1@�@�33@�hs@��@��@���@�A�@�n�@�J@��@�@�r�@��@܃@�~�@��@�  @��
@׶F@�b@��y@Չ7@�(�@�5?@��#@���@ѩ�@�G�@�Z@ύP@�^5@�-@�l�@�C�@ͺ^@�  @��
@�J@�Z@�=q@þw@Å@Õ�@å�@�S�@�V@�@�x�@��9@�"�@��R@�=q@���@��D@�1'@�9X@�  @�
=@���@��`@��@�b@��@���@���@�S�@���@��@���@���@���@��T@��@��u@�  @��
@���@�S�@��R@�5?@��@���@��9@��@�9X@��;@�t�@�ȴ@�M�@��T@�@��^@���@�hs@�?}@�%@��/@��/@�Ĝ@�Ĝ@�z�@�1@��
@��@���@�t�@�@�n�@��@�@���@��7@�O�@��@��@��w@��@���@�S�@�;d@�+@��H@�E�@�$�@���@���@���@�O�@���@���@��@���@��@�9X@�1@��
@��w@��P@�C�@�"�@�@��R@�^5@��@��@��^@���@�`B@�%@��/@��9@�bN@��@���@��m@��@�S�@��@�V@�=q@�$�@�{@��@��h@�p�@�X@��@��@�r�@�Q�@�1@��F@�t�@�dZ@�"�@��H@���@�v�@�5?@���@���@�O�@���@���@�j@�A�@�t�@�@��y@��@���@���@���@�~�@�ff@�=q@�J@��@��@�bN@�(�@��;@�
=@��@��R@�v�@�@�@���@��@�G�@��/@� �@�  @���@�t�@�+@��y@��H@���@��+@�5?@���@��T@�@�x�@�O�@�&�@��u@�I�@�9X@�1@���@�dZ@�K�@�
=@���@���@��!@�~�@�-@�J@�@�@�G�@���@�I�@�ƨ@��@�t�@�\)@�C�@�"�@�@��y@�ȴ@���@�{@��T@���@���@��@�V@�M�@�@���@�O�@���@��`@��/@���@�Ĝ@��9@��u@�j@�I�@�1'@�  @��m@��
@��w@��@�+@��R@���@�V@�$�@��T@���@��h@��7@�x�@�G�@���@���@��j@��@��D@�r�@��@K�@
=@~�y@~�+@~{@}�-@}`B@}V@|�@|(�@{�
@{��@{t�@z�@zn�@z�@zJ@y�^@y�7@y7L@x��@x��@x�9@x1'@x  @w�w@w\)@wK�@v��@u�T@u/@t�@t�@tz�@s"�@r~�@rn�@r^5@q��@q�7@p�`@pb@o��@oK�@o�@o
=@o
=@nE�@m�T@l�@lj@l(�@k��@k�m@k33@j~�@j^5@i��@i�7@i�@i%@h��@h�u@hA�@hb@g�w@f�y@f��@fff@fE�@fE�@f5?@f{@e��@e`B@e?}@d��@d��@d�j@c��@c�@cdZ@cC�@b��@a��@a&�@`��@`�u@` �@_��@_l�@^�R@^5?@]��@]`B@]`B@]O�@]�@\�@\9X@[��@[S�@Z�H@Zn�@Z^5@Z=q@Z�@Y�#@Y��@YX@Y7L@X�`@Xr�@XQ�@XA�@XA�@W��@Wl�@WK�@W+@Vȴ@U��@Up�@Up�@T�/@Tz�@T(�@S�F@SdZ@SS�@R��@R�\@Q��@Q��@Q�@PĜ@P�9@PQ�@O��@O\)@O
=@Nȴ@N�+@NV@N@Mp�@L��@Lz�@K�m@K��@K��@Kt�@K33@J��@J^5@I�@I��@IG�@I%@HĜ@H��@H�u@H�@Hr�@HA�@G�@G�w@Gl�@G;d@G
=@F�y@Fȴ@Fv�@FE�@E�T@E�-@E�@EO�@D�/@D�D@DZ@D(�@Cƨ@C��@C�@CdZ@C"�@B�H@B��@Bn�@BM�@BJ@A�@A��@@��@@�@?�;@?|�@?+@>ȴ@>��@>�+@=�T@=��@=�@=�@<�@<Z@<(�@<�@<1@;�m@;��@;�@;�@;dZ@;"�@:�H@:~�@:-@9�^@9��@9hs@9�@8��@8�9@8�9@8Q�@8b@7�P@7�@7
=@6�y@6��@6v�@5@5O�@4��@4�/@4�j@4�@4�D@4z�@4j@4(�@3��@3�
@3��@3"�@2�H@2�!@2~�@2n�@2M�@2=q@2�@1�#@1��@1��@1�^@1��@1G�@1&�@1&�@1�@1%@0��@0�u@0�@0b@/��@/K�@/+@/�@.�y@.�R@.��@.��@.@-�h@-`B@-�@,��@,�@,�D@,(�@+��@+��@+dZ@+C�@+33@+@*�@*�H@*n�@*^5@*M�@*=q@)��@)�#@)��@)��@)��@)��@)x�@)X@)�@(��@(��@(Ĝ@(��@(�u@(�@(Q�@(Q�@(1'@'�;@'��@'\)@';d@'
=@&�@&��@&ff@&@%�-@%�-@%�h@%p�@%?}@%�@$��@$�@$�/@$�@$j@$I�@$�@$1@$1@#�
@#�
@#�F@#�@#C�@"�@"�\@"M�@"-@"�@"J@!�@!�^@!x�@!G�@ �`@ ��@ Ĝ@ �9@ ��@ ��@ �u@ r�@ Q�@�@��@l�@+@�y@��@v�@E�@{@@�@��@�-@�h@O�@V@�/@z�@Z@(�@��@�
@�F@��@C�@33@"�@o@�@��@�!@n�@J@��@��@�@�#@��@hs@7L@&�@%@��@��@�9@r�@b@  @�@��@�P@
=@��@�y@�@�R@��@v�@E�@E�@$�@�T@��@@��@��@p�@/@��@��@�/@��@��@��@�j@��@Z@(�@�@��@�
@�F@��@��@��@�@S�@o@��@��@^5@�@�@��@�7@x�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BK�BK�BM�BN�BM�BM�BM�BL�BL�BL�BL�BL�BL�BL�BM�BL�BK�BH�B>wB!�BB2-B=qBN�BaHB|�B�B�-B�5B�BoB�BoB1B��B�TB��B�
B�B��BB+BB1BhB�B%�B33B<jBA�BC�BC�BF�BC�BR�BZBS�BF�BI�BO�BD�BC�BiyB�PB�\B�7B�+B�B�B~�Bv�BbNB^5BI�B#�B�BB�B�TB�B��BǮB��B�B�oBw�Bm�B_;B@�B�BhB
��B
�B
�TB
�fB1B.B7LB:^B8RB-B�B
��B
�;B
��B
�9B
��B
��B
��B
��B
�B
r�B
bNB
R�B
0!B
�B
(�B
+B
!�B
VB
B	��B	�B	�BB	�B	��B	�RB	��B	�DB	�7B	�%B	t�B	`BB	\)B	S�B	E�B	-B	�B	�B	�B	1B	%B��B�B�B��B��BǮB�-BÖBÖB�}B�XB�B��B��B��B��B�bB��B��B��B��B��B�{B�oB�hB�VB�%B�bB�=B�B�Bz�Bs�Bq�Bs�Bq�Bn�Bk�BgmBe`Be`BhsBiyBjBn�Bq�Bo�Bl�Bk�Bp�Bt�Bu�Br�Bm�Bk�BjBk�Bk�Bk�Bn�Bp�Bu�Bw�Bz�B{�B}�Bz�B~�B�B|�Bu�B� B� B{�B~�B}�Bu�Bt�Bn�Bw�Bx�B{�By�Bt�Bw�Bv�B{�Bt�Bs�Bu�Bw�Br�Bp�Bk�BbNBT�B^5BdZBjBw�Bo�Bk�Bp�Bo�Bp�Bp�Bs�Bs�Bu�B~�Bz�Bx�Bu�B}�B�B�B�%B�7B�DB�VB�oB��B��B��B��B��B��B��B��B��B��B�B�B�B�!B�FB�FB�FB�9B�dB�wB�qB�dB�}BÖBÖB��BBƨB��B��B�
B�B�B�B�
B�)B�TB�`B�ZB�ZB�TB�NB�yB�B�B�B�B�B��B��B	B	B	DB	\B	bB	bB	uB	�B	�B	�B	�B	�B	�B	"�B	$�B	(�B	)�B	-B	0!B	1'B	33B	5?B	5?B	7LB	:^B	=qB	A�B	D�B	D�B	E�B	F�B	G�B	E�B	M�B	Q�B	R�B	S�B	XB	YB	XB	ZB	aHB	bNB	dZB	gmB	gmB	jB	p�B	r�B	s�B	s�B	t�B	x�B	{�B	~�B	� B	� B	�B	�B	�B	�1B	�JB	�VB	�bB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�-B	�3B	�9B	�9B	�?B	�RB	�RB	�RB	�XB	�qB	�wB	�}B	��B	ÖB	ǮB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�#B	�#B	�B	�B	�B	�B	�;B	�BB	�BB	�BB	�`B	�fB	�fB	�fB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
	7B
1B
+B
1B
1B
	7B

=B
DB
DB
DB
DB

=B

=B

=B
JB
JB
PB
PB
PB
JB
JB
PB
\B
\B
\B
\B
bB
hB
hB
hB
hB
hB
uB
{B
uB
uB
uB
oB
oB
{B
�B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
"�B
"�B
!�B
!�B
"�B
"�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
$�B
#�B
%�B
&�B
'�B
'�B
'�B
'�B
&�B
'�B
'�B
'�B
(�B
'�B
&�B
'�B
)�B
(�B
(�B
(�B
(�B
)�B
+B
+B
+B
,B
+B
,B
,B
.B
/B
.B
.B
-B
-B
.B
-B
/B
/B
1'B
1'B
1'B
1'B
1'B
1'B
2-B
1'B
1'B
33B
33B
33B
2-B
2-B
33B
33B
2-B
1'B
33B
49B
33B
49B
5?B
5?B
5?B
5?B
5?B
6FB
6FB
7LB
7LB
7LB
8RB
8RB
7LB
8RB
:^B
;dB
;dB
;dB
;dB
;dB
;dB
<jB
<jB
>wB
?}B
?}B
?}B
>wB
?}B
?}B
@�B
@�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
A�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
F�B
G�B
G�B
I�B
J�B
J�B
K�B
J�B
J�B
K�B
K�B
K�B
K�B
L�B
M�B
N�B
N�B
N�B
M�B
N�B
N�B
N�B
N�B
M�B
N�B
N�B
N�B
P�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
R�B
R�B
R�B
Q�B
Q�B
R�B
S�B
T�B
T�B
VB
T�B
VB
T�B
T�B
T�B
VB
T�B
T�B
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
W
B
XB
YB
XB
XB
XB
XB
XB
XB
XB
YB
ZB
ZB
ZB
ZB
ZB
ZB
YB
YB
ZB
[#B
ZB
[#B
[#B
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
^5B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
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
`BB
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
aHB
bNB
bNB
bNB
bNB
bNB
bNB
cTB
bNB
bNB
bNB
cTB
cTB
cTB
dZB
cTB
cTB
cTB
cTB
cTB
cTB
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
ffB
gmB
ffB
gmB
gmB
ffB
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
iyB
iyB
hsB
hsB
iyB
hsB
iyB
iyB
iyB
jB
jB
jB
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
k�B
k�B
l�B
l�B
l�B
l�B
l�B
k�B
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
m�B
m�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
o�B
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
r�B
r�B
r�B
r�B
r�B
s�B
t�B
t�B
t�B
u�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BK�BK�BM�BN�BM�BM�BM�BL�BL�BL�BL�BL�BL�BL�BM�BL�BL0BJ#BB�G�O�B�B3�B@BQhBc�B� B��B�hBߊB�B@BB�B
�B�DB�BՁB�1B��B�0B�B	B�B
�B�B�B'�B4�B=�BB�BEBEBHfBF�BU�B[	BU�BI�BK^BQ�BG�BE�Bj�B��B��B�rB�B��B��B�iBx�Bd�B`\BM�B(�BB�B�B�zBۦBӏBɆB�JB��B�$B|�Bq'Ba�BD�B"�B�B
�B
�-B
�B
�RB+B-]B7fB;dB;JB0�B"�B
�6B
��B
�.B
�lB
�QB
��B
��B
��B
�B
u%B
dZB
U�B
5%B
pG�O�G�O�G�O�B
.B
�B	��B	�3B	��B	��B	͹B	�B	��B	��B	�rB	�zB	wLB	cnB	]�B	VmB	IB	1vB	"4B	!�B	�B	B	KB	 OB��B��BרB֡B��B�BāB�3B��B��B�B��B��B��B�8G�O�B��B��B�_B�
B�yB��B��B��B�HB��B�B��B�mB��B|�Bu�Bs�Bu�BsMBqABm�Bi�Bf�Bf�Bh�Bi�Bj�BnIBr�Bp�BnBl�BqvBu?BvBs3Bn�Bl�Bk�Bl�Bl�Bl�Bo�Bq�Bv�Bx�B{�B|�B~�B|B�B��B}�Bw�B��B��B|�B}B~�BwLBvFBpUBx8ByXB|6Bz�Bv+Bx�Bw�B|�Bv+Bt�Bv`BxlBtBq�Bl�BdZBW$B_;Bd�BjeBxlBqvBm)Bq�BqBq�BqvBtBs�Bu�B�B|By�Bv�B~BB�-B�{B��B�	B��B��B��B�hB�TB��B��B�TB�'B�B�5B�5B�*B�B�)B��B��B��B��B��B�?B��B��B��B�PB��B��B�B�UB�aB�_B�(B�:B�$B�KB�_B�_BרBܒB�B�zB�tB�B�B�B��B�B��B�B�;B�B�`B�qB	GB	SB	�B	�B	�B	 B	�B	�B	�B	�B	�B	B	�B	# B	%B	)B	*B	-CB	0�B	1�B	3hB	5tB	5tB	7�B	:�B	=�B	A�B	D�B	D�B	E�B	F�B	HB	F?B	NB	RB	S&B	T,B	XEB	Y1B	X_B	Z�B	a|B	b�B	d�B	g�B	g�B	j�B	p�B	r�B	s�B	s�B	uB	y	B	|B	.B	�4B	�OB	�-B	�gB	�mB	��B	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�,B	�sB	�]B	�GB	�MB	�nB	�nB	�tB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�HB	�NB	�@B	�&B	�oB	�SB	�B	�7B	�7B	�7B	�QB	�=B	�=B	�kB	�kB	ٚB	ڠB	ߊB	��B	��B	��B	�zB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	� B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	��B	��B	�B	�B	�B	�	B	�$B	�B	�B	�B	�B	�B	�"B	�B	�6B	�JB	�JB	�PB	�cB
[B
-B
3B
3B
3B
3B
3B
3B
gB
�B
MB
9B
%B
B
	B
KB
�B
�B
�B
	lB

XB
DB
^B
^B
xB

XB

XB

XB
dB
dB
jB
jB
jB
~B
�B
�B
vB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
B
�B
 �B
 �B
�B
B
�B
 B
!B
"�B
#B
#B
"4B
"B
"�B
# B
$&B
$B
%B
$�B
$�B
$�B
%B
%,B
$@B
%�B
'B
'�B
'�B
($B
(
B
'8B
($B
($B
($B
(�B
($B
'8B
($B
*0B
)B
)DB
)_B
)DB
*0B
+B
+6B
+6B
,=B
+QB
,=B
,=B
.IB
/B
./B
./B
-]B
-CB
.IB
-]B
/OB
/OB
1AB
1AB
1AB
1AB
1AB
1AB
2GB
1AB
1vB
33B
3MB
3hB
2|B
2GB
3MB
3MB
2|B
1�B
3MB
4nB
3hB
4nB
5ZB
5�B
5tB
5ZB
5�B
6`B
6zB
7�B
7�B
7fB
8lB
8�B
7�B
8�B
:�B
;�B
;�B
;B
;�B
;�B
;�B
<�B
<�B
>�B
?}B
?�B
?�B
>�B
?�B
?�B
@�B
@�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
A�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
GB
G�B
G�B
I�B
J�B
J�B
K�B
J�B
KB
K�B
K�B
K�B
K�B
L�B
M�B
N�B
OB
N�B
NB
N�B
N�B
N�B
N�B
M�B
OB
N�B
O(B
P�B
PB
PB
Q B
Q B
Q B
QB
Q B
Q4B
Q4B
R�B
S&B
S&B
R B
RTB
S@B
TB
U2B
UB
VB
U2B
VB
UB
UB
U2B
VB
UB
U2B
V9B
W$B
W$B
W
B
W?B
W$B
W$B
W?B
XB
X+B
XB
XEB
W?B
XEB
Y1B
X+B
X+B
XEB
X+B
X+B
XEB
XEB
YKB
Z7B
Z7B
Z7B
Z7B
Z7B
Z7B
YKB
YeB
ZQB
[=B
Z7B
[WB
[=B
[=B
[=B
[WB
\]B
]IB
]IB
]IB
]IB
]dB
]IB
^5B
^OB
^OB
^jB
^OB
^OB
^OB
_;B
^OB
^jB
^OB
^OB
_VB
_VB
_VB
_VB
_;B
_VB
_pB
`BB
_VB
_pB
_VB
`vB
`vB
`vB
`\B
`\B
`\B
`vB
a|B
bNB
bhB
bhB
b�B
b�B
bhB
cTB
b�B
bhB
bhB
cnB
cnB
cnB
dtB
c�B
cnB
c�B
cnB
cnB
c�B
dtB
dtB
e`B
e`B
ezB
ezB
e�B
e�B
e�B
e�B
f�B
gmB
ffB
gmB
gmB
f�B
f�B
f�B
f�B
f�B
g�B
g�B
g�B
g�B
h�B
h�B
h�B
iyB
i�B
h�B
h�B
i�B
h�B
i�B
i�B
i�B
j�B
j�B
j�B
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
k�B
k�B
l�B
l�B
l�B
l�B
l�B
k�B
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
m�B
m�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
o�B
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
r�B
r�B
r�B
r�B
r�B
s�B
t�B
t�B
t�B
u�11111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.07(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201709050038422017090500384220170905003842201806221318312018062213183120180622131831201804050720532018040507205320180405072053  JA  ARFMdecpA19c                                                                20170901123506  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20170901033508  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20170901033510  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20170901033510  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20170901033511  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20170901033511  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20170901033511  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20170901033511  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20170901033511  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20170901033512                      G�O�G�O�G�O�                JA  ARUP                                                                        20170901035515                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20170901153518  CV  JULD            G�O�G�O�F��                JM  ARSQJMQC2.0                                                                 20170904000000  CF  PSAL_ADJUSTED_QCB(  C�  G�O�                JM  ARCAJMQC2.0                                                                 20170904153842  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20170904153842  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404222053  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622041831  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116221516                      G�O�G�O�G�O�                