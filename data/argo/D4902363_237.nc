CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-05-08T00:35:24Z creation;2018-05-08T00:35:30Z conversion to V3.1;2019-12-19T07:42:53Z update;     
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
_FillValue                 �  I    PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `l   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �<   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ܘ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �(   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �(   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �(   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �(   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �    HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �$   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �(Argo profile    3.1 1.2 19500101000000  20180508003524  20200115131516  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0576_237                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�`��� 1   @�`��@y�@:?v_ح��d[�*0U21   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @9��@�  @�  A   A   A@  A^ffA�  A�  A�33A�  A�33A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@ffBH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ�CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl�Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D���D�@ D؀ D�� D�  D�@ Dـ D�� D�3D�C3Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�3D�<�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @5�@{�@�@�A�HA>�HA]G�A~�HA�p�A���A�p�A���A�p�A�p�A�p�A�p�B�RB�RB�RB�RB'�RB/�RB7�RB@�BG�RBO�RBW�RB_�RBg�RBo�RBw�RB�RB��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CJ�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Cl�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
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
D {�D ��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D	{�D	��D
{�D
��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D��D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D {�D ��D!{�D!��D"{�D"��D#{�D#��D${�D$��D%{�D%��D&{�D&��D'{�D'��D({�D(��D){�D)��D*{�D*��D+{�D+��D,{�D,��D-{�D-��D.{�D.��D/{�D/��D0{�D0��D1{�D1��D2{�D2��D3{�D3��D4{�D4��D5{�D5��D6{�D6��D7{�D7��D8{�D8��D9{�D9��D:{�D:��D;{�D;��D<{�D<��D={�D=��D>{�D>��D?{�D?��D@{�D@��DA{�DA��DB{�DB��DC{�DC��DD{�DD��DE{�DE��DF{�DF��DG{�DG��DH{�DH��DI{�DI��DJ{�DJ��DK{�DK��DL{�DL��DM{�DM��DN{�DN��DO{�DO��DP{�DP��DQ{�DQ��DR{�DR��DS{�DS��DT{�DT��DU{�DU��DV{�DV��DW{�DW��DX{�DX��DY{�DY��DZ{�DZ��D[{�D[��D\{�D\��D]{�D]��D^{�D^��D_{�D_��D`{�D`��Da{�Da��Db{�Db��Dc{�Dc��Dd{�Dd��De{�De��Df{�Df��Dg{�Dg��Dh{�Dh��Di{�Di��Dj{�Dj��Dk{�Dk��Dl{�Dl��Dm{�Dm��Dn{�Dn��Do{�Do��Dp{�Dp��Dq{�Dq��Dr{�Dr��Ds{�Ds��Dt{�Dt��Du{�Du��Dv{�Dv��Dw{�Dw��Dx{�Dx��Dy{�Dy��Dz{�Dz��D{{�D{��D|{�D|��D}{�D}��D~{�D~��D{�D��D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D� �D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D½�D���D�=�D�}�Dý�D���D�=�D�}�DĽ�D���D�=�D�}�DŽ�D���D�=�D�}�Dƽ�D���D�=�D�}�Dǽ�D���D�=�D�}�DȽ�D���D�=�D�}�Dɽ�D���D�=�D�}�Dʽ�D���D�=�D�}�D˽�D���D�=�D�}�D̽�D���D�=�D�}�Dͽ�D���D�=�D�}�Dν�D���D�=�D�}�DϽ�D���D�=�D�}�Dн�D���D�=�D�}�Dѽ�D���D�=�D�}�Dҽ�D���D�=�D�}�Dӽ�D���D�=�D�}�DԽ�D���D�=�D�}�Dս�D���D�=�D�}�Dֽ�D���D�=�D�}�D׽�D���D�=�D�}�Dؽ�D���D�=�D�}�Dٽ�D� �D�@�D�}�Dڽ�D���D�=�D�}�D۽�D���D�=�D�}�Dܽ�D���D�=�D�}�Dݽ�D���D�=�D�}�D޽�D���D�=�D�}�D߽�D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D���D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D� �D�:�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��/A���A�A��wA��FA��RA��FA��9A��-A��9A��-A��!A��9A��9A��!A��A��A��9A��FA��9A��A���A��A��A��A���A���A���A���A���A��!A��!A�O�A�5?A�$�A�oA���A��9A��hA��PA�v�A�C�A��-A��;A�$�A�I�A�ȴA���A�ȴA��A�"�A��A��yA�(�A���A��
A�/A�ffA�?}A��A�hsA��#A�p�A��7A�v�A�"�A� �A�C�A�XA�hsA��A���A���A�Q�A�/A��A��A��FA���A�dZA��A�9XA���A�x�A�{AS�A~��A}�;A|��A|A{�wA{K�Az��Az �AyVAw�#Aw��Av��Av�AvM�Av�Au��Au
=Ar��Ap�HAo\)An�HAl �AiƨAh��Ag�wAg"�AeG�Ac�^Ab~�Aa�wAaK�A`��A_��A^��A^��A^$�A]C�A\n�AZv�AW��AV5?AT�9ATQ�AT5?AS�AS��AS/ARI�AQ?}AO"�AL��AJ�AIƨAI��AI&�AH�jAH=qAG��AF��AF�AE��AE�AE
=AD�HAD��ADJAC�wACt�AB�AB1'AAp�A@ffA?x�A?�A>n�A<��A9�A8E�A7G�A6��A6bNA6  A533A4Q�A45?A4-A4(�A4bA3��A3�^A2$�A0�`A/�-A.��A-|�A,ĜA+�#A*I�A)�A(�9A'l�A&�/A&�A%��A$�\A#K�A 5?A�PAC�A�+A��A�wAS�A�`AA�HA$�A;dA^5A�7A��A�TAG�A=qA33AM�A�A��A=qA  A��A��A��A(�AA
r�A
bA	�PA�AM�A �A��A��AA�A�-AVAz�A �A7LAĜAA�A�;A�-AXA"�@��!@�
=@��h@�Q�@���@�9X@�S�@��#@�V@�r�@�dZ@���@�Z@�@�5?@�p�@��@�-@�j@��
@�^5@�7L@�9X@��@ް!@�z�@�ff@�`B@���@�v�@�O�@�9X@ӕ�@��@�v�@ѩ�@Ь@��@��@Ͳ-@��@�Z@��m@�ȴ@ț�@�C�@Ɨ�@�E�@���@���@�K�@�n�@�{@��@�S�@��@�V@�G�@��@���@�@���@���@�S�@��H@���@�=q@�hs@��9@� �@�1@��P@�n�@�r�@��@���@��@��@�G�@���@�j@��m@��@��#@�7L@��/@�r�@�I�@��w@��\@�5?@�`B@�j@��@���@�v�@�5?@��7@��/@��
@�C�@���@��+@�^5@�$�@��-@���@���@�G�@�Z@���@��R@�5?@��^@�X@�G�@��`@���@�1'@�t�@�S�@�
=@���@�E�@��@�G�@��`@��@���@��@�\)@�
=@�=q@�@��@���@���@�+@��@��T@��@���@��D@��@��w@�33@��@���@�ff@�=q@�X@���@�r�@�Q�@�b@��;@��w@���@�dZ@�M�@��@��-@���@��@�&�@���@���@�z�@��@��@��w@�|�@�K�@�o@�ȴ@���@�v�@�V@�$�@��@���@���@��h@��7@�`B@�G�@��@��@��D@�Z@�1'@�(�@��@�  @�P@\)@|�@\)@~�+@}�@}�T@}��@}@}��@}O�@|�@{33@{33@{"�@z~�@yG�@y%@x��@x�`@xĜ@x��@y%@y��@y�7@yG�@x��@x��@x �@w;d@v��@v��@wK�@w\)@wK�@w�@vV@v{@v@u�h@up�@u?}@u�@uV@t��@t��@t�@t�D@t9X@t1@sC�@s@r�H@r�H@r~�@q�@q�@p��@p�9@pr�@p1'@o�@o��@o�P@n��@nv�@n�+@nv�@nV@n{@m��@m��@mp�@m`B@mO�@m?}@m/@m/@m�@l��@l��@lz�@l�@k��@kS�@j~�@i��@iG�@i7L@i7L@hĜ@hQ�@h  @gK�@f��@f��@f�+@fff@fV@f5?@f{@e��@eO�@e/@d�/@dz�@d(�@c33@c@b��@b��@bM�@a�#@a��@a�7@aX@a&�@`��@`��@`Q�@` �@_�w@_l�@_K�@_;d@_�@^E�@]�T@]p�@\��@\��@\�D@[�@Z�H@ZM�@Y�#@Y��@Y7L@X�`@XbN@W�@W+@Vff@V@U@U/@T��@T9X@S��@S��@SS�@R��@R~�@R=q@RJ@Q�@Q�^@Q�7@QG�@Q�@PĜ@P�9@P�@P �@Pb@O�@O\)@O
=@N�R@Nff@NE�@N@M@M`B@M�@L��@K�m@K�m@K�F@K33@J�H@J��@JM�@JJ@I��@H��@H��@HQ�@Hb@G��@G�@G��@G\)@F�y@F��@FE�@F5?@F{@F@E@E��@E?}@D��@D��@Dj@DZ@D(�@D1@Cƨ@C��@C��@CS�@B��@Bn�@B=q@A��@Ax�@Ax�@Ahs@A%@@�`@@��@@1'@?�;@?|�@?l�@?l�@?\)@?�@>�y@>�R@>�+@>5?@=�@=�T@=�-@=p�@=O�@=V@<�j@<Z@<�@;�m@;ƨ@;ƨ@;�@;@:��@:��@:^5@:�@9��@9&�@8�9@8Q�@81'@7�@7�P@7l�@7K�@7�@7�@7
=@6�@6��@6��@6ff@65?@5��@4��@4�@4�D@4Z@4I�@4I�@49X@4(�@41@3�m@3ƨ@3t�@3o@2�!@2-@1�^@1G�@1�@0��@0Ĝ@0�u@0�@0r�@0A�@/�P@/l�@/l�@/\)@/K�@/+@.�R@.ff@.E�@.@-��@-p�@-O�@-V@,z�@,z�@,z�@,z�@,Z@,(�@,1@+�
@+"�@*��@*�!@*��@*��@*��@*�\@*=q@*J@)��@)X@)�@(�9@(bN@(  @'�@'
=@&ȴ@&ȴ@&ȴ@&ȴ@&ff@&$�@%�@%��@%�@%p�@%`B@%?}@%�@$�/@$�j@$�j@$�j@$�D@$�D@$Z@$9X@$�@#ƨ@#�@#@"�!@"^5@"-@"J@!��@!�7@!X@!�@ �`@ �9@ �@ A�@��@K�@
=@�y@ȴ@��@v�@ff@V@@/@�@�@�/@z�@�@1@�
@�@t�@dZ@dZ@dZ@t�@t�@dZ@C�@@��@~�@=q@�@�@�#@��@��@hs@&�@�`@r�@A�@ �@��@�P@�P@|�@l�@\)@\)@;d@��@��@ȴ@��@ff@�@p�@O�@?}@/@�/@(�@1@�m@�m@ƨ@t�@�@n�@=q@J@�@�#@��@hs@hs@X@%@�@1'@b@�@�@|�@|�@|�@l�@+@�@�@�y@��@��@��@p�@/@��@�@Z@9X@1@1@��@�
@�F@��@t�@C�@o@
M�@
J@	��@��@Ĝ@Ĝ@�9@�9@�9@�9@�9@�9@��@��@��@�@A�@ �@b@b@  @  @  @  @  @  @  @�@�@�;@��@|�@ȴ@�+@v�@v�@v�@E�@��@��@p�@O�@/@�@V@�@�@�/@��@�/@�j@z�@Z@I�@I�@(�@�m@ƨ@��@�@t�@33@��@��@~�@n�@M�@�@��@��@��@�7@hs@7L@�@ �`@ ��@ Ĝ@ �9@ ��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��/A���A�A��wA��FA��RA��FA��9A��-A��9A��-A��!A��9A��9A��!A��A��A��9A��FA��9A��A���A��A��A��A���A���A���A���A���A��!A��!A�O�A�5?A�$�A�oA���A��9A��hA��PA�v�A�C�A��-A��;A�$�A�I�A�ȴA���A�ȴA��A�"�A��A��yA�(�A���A��
A�/A�ffA�?}A��A�hsA��#A�p�A��7A�v�A�"�A� �A�C�A�XA�hsA��A���A���A�Q�A�/A��A��A��FA���A�dZA��A�9XA���A�x�A�{AS�A~��A}�;A|��A|A{�wA{K�Az��Az �AyVAw�#Aw��Av��Av�AvM�Av�Au��Au
=Ar��Ap�HAo\)An�HAl �AiƨAh��Ag�wAg"�AeG�Ac�^Ab~�Aa�wAaK�A`��A_��A^��A^��A^$�A]C�A\n�AZv�AW��AV5?AT�9ATQ�AT5?AS�AS��AS/ARI�AQ?}AO"�AL��AJ�AIƨAI��AI&�AH�jAH=qAG��AF��AF�AE��AE�AE
=AD�HAD��ADJAC�wACt�AB�AB1'AAp�A@ffA?x�A?�A>n�A<��A9�A8E�A7G�A6��A6bNA6  A533A4Q�A45?A4-A4(�A4bA3��A3�^A2$�A0�`A/�-A.��A-|�A,ĜA+�#A*I�A)�A(�9A'l�A&�/A&�A%��A$�\A#K�A 5?A�PAC�A�+A��A�wAS�A�`AA�HA$�A;dA^5A�7A��A�TAG�A=qA33AM�A�A��A=qA  A��A��A��A(�AA
r�A
bA	�PA�AM�A �A��A��AA�A�-AVAz�A �A7LAĜAA�A�;A�-AXA"�@��!@�
=@��h@�Q�@���@�9X@�S�@��#@�V@�r�@�dZ@���@�Z@�@�5?@�p�@��@�-@�j@��
@�^5@�7L@�9X@��@ް!@�z�@�ff@�`B@���@�v�@�O�@�9X@ӕ�@��@�v�@ѩ�@Ь@��@��@Ͳ-@��@�Z@��m@�ȴ@ț�@�C�@Ɨ�@�E�@���@���@�K�@�n�@�{@��@�S�@��@�V@�G�@��@���@�@���@���@�S�@��H@���@�=q@�hs@��9@� �@�1@��P@�n�@�r�@��@���@��@��@�G�@���@�j@��m@��@��#@�7L@��/@�r�@�I�@��w@��\@�5?@�`B@�j@��@���@�v�@�5?@��7@��/@��
@�C�@���@��+@�^5@�$�@��-@���@���@�G�@�Z@���@��R@�5?@��^@�X@�G�@��`@���@�1'@�t�@�S�@�
=@���@�E�@��@�G�@��`@��@���@��@�\)@�
=@�=q@�@��@���@���@�+@��@��T@��@���@��D@��@��w@�33@��@���@�ff@�=q@�X@���@�r�@�Q�@�b@��;@��w@���@�dZ@�M�@��@��-@���@��@�&�@���@���@�z�@��@��@��w@�|�@�K�@�o@�ȴ@���@�v�@�V@�$�@��@���@���@��h@��7@�`B@�G�@��@��@��D@�Z@�1'@�(�@��@�  @�P@\)@|�@\)@~�+@}�@}�T@}��@}@}��@}O�@|�@{33@{33@{"�@z~�@yG�@y%@x��@x�`@xĜ@x��@y%@y��@y�7@yG�@x��@x��@x �@w;d@v��@v��@wK�@w\)@wK�@w�@vV@v{@v@u�h@up�@u?}@u�@uV@t��@t��@t�@t�D@t9X@t1@sC�@s@r�H@r�H@r~�@q�@q�@p��@p�9@pr�@p1'@o�@o��@o�P@n��@nv�@n�+@nv�@nV@n{@m��@m��@mp�@m`B@mO�@m?}@m/@m/@m�@l��@l��@lz�@l�@k��@kS�@j~�@i��@iG�@i7L@i7L@hĜ@hQ�@h  @gK�@f��@f��@f�+@fff@fV@f5?@f{@e��@eO�@e/@d�/@dz�@d(�@c33@c@b��@b��@bM�@a�#@a��@a�7@aX@a&�@`��@`��@`Q�@` �@_�w@_l�@_K�@_;d@_�@^E�@]�T@]p�@\��@\��@\�D@[�@Z�H@ZM�@Y�#@Y��@Y7L@X�`@XbN@W�@W+@Vff@V@U@U/@T��@T9X@S��@S��@SS�@R��@R~�@R=q@RJ@Q�@Q�^@Q�7@QG�@Q�@PĜ@P�9@P�@P �@Pb@O�@O\)@O
=@N�R@Nff@NE�@N@M@M`B@M�@L��@K�m@K�m@K�F@K33@J�H@J��@JM�@JJ@I��@H��@H��@HQ�@Hb@G��@G�@G��@G\)@F�y@F��@FE�@F5?@F{@F@E@E��@E?}@D��@D��@Dj@DZ@D(�@D1@Cƨ@C��@C��@CS�@B��@Bn�@B=q@A��@Ax�@Ax�@Ahs@A%@@�`@@��@@1'@?�;@?|�@?l�@?l�@?\)@?�@>�y@>�R@>�+@>5?@=�@=�T@=�-@=p�@=O�@=V@<�j@<Z@<�@;�m@;ƨ@;ƨ@;�@;@:��@:��@:^5@:�@9��@9&�@8�9@8Q�@81'@7�@7�P@7l�@7K�@7�@7�@7
=@6�@6��@6��@6ff@65?@5��@4��@4�@4�D@4Z@4I�@4I�@49X@4(�@41@3�m@3ƨ@3t�@3o@2�!@2-@1�^@1G�@1�@0��@0Ĝ@0�u@0�@0r�@0A�@/�P@/l�@/l�@/\)@/K�@/+@.�R@.ff@.E�@.@-��@-p�@-O�@-V@,z�@,z�@,z�@,z�@,Z@,(�@,1@+�
@+"�@*��@*�!@*��@*��@*��@*�\@*=q@*J@)��@)X@)�@(�9@(bN@(  @'�@'
=@&ȴ@&ȴ@&ȴ@&ȴ@&ff@&$�@%�@%��@%�@%p�@%`B@%?}@%�@$�/@$�j@$�j@$�j@$�D@$�D@$Z@$9X@$�@#ƨ@#�@#@"�!@"^5@"-@"J@!��@!�7@!X@!�@ �`@ �9@ �@ A�@��@K�@
=@�y@ȴ@��@v�@ff@V@@/@�@�@�/@z�@�@1@�
@�@t�@dZ@dZ@dZ@t�@t�@dZ@C�@@��@~�@=q@�@�@�#@��@��@hs@&�@�`@r�@A�@ �@��@�P@�P@|�@l�@\)@\)@;d@��@��@ȴ@��@ff@�@p�@O�@?}@/@�/@(�@1@�m@�m@ƨ@t�@�@n�@=q@J@�@�#@��@hs@hs@X@%@�@1'@b@�@�@|�@|�@|�@l�@+@�@�@�y@��@��@��@p�@/@��@�@Z@9X@1@1@��@�
@�F@��@t�@C�@o@
M�@
J@	��@��@Ĝ@Ĝ@�9@�9@�9@�9@�9@�9@��@��@��@�@A�@ �@b@b@  @  @  @  @  @  @  @�@�@�;@��@|�@ȴ@�+@v�@v�@v�@E�@��@��@p�@O�@/@�@V@�@�@�/@��@�/@�j@z�@Z@I�@I�@(�@�m@ƨ@��@�@t�@33@��@��@~�@n�@M�@�@��@��@��@�7@hs@7L@�@ �`@ ��@ Ĝ@ �9@ ��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�NB�BB�BB�)B�B��B��B��B��BƨB�3B��B�bB^5B2-B"�BoB��B��BɺBŢBŢB�wB��B�B^5BaHBW
BF�B�BDBB
�B
�;B
��B
��B
�}B
�FB
�?B
�XB
�FB
�!B
�'B
�B
�B
��B
��B
��B
�{B
�7B
�+B
�B
{�B
x�B
r�B
n�B
dZB
dZB
e`B
`BB
ZB
R�B
J�B
@�B
E�B
B�B
=qB
>wB
:^B
33B
&�B
bB
B	��B	��B	�HB	��B	�/B	��B	��B	��B	�LB	�RB	�RB	�XB	�3B	�B	��B	�B	��B	��B	�oB	�B	p�B	o�B	hsB	l�B	n�B	k�B	e`B	_;B	P�B	I�B	7LB	)�B	�B	0!B	0!B	+B	(�B	$�B	!�B	�B	�B	�B	�B	�B	�B	{B	bB	\B	DB	+B	B��B�B�B�B�sB�BÖBǮB��B��B��B��B��BƨB��B��B��B��B��BÖB�3B�B�B�B��B��B��B��B��B��B�\B��B�hB�\B�+By�BffB|�B�Bx�BiyBo�Bu�Bp�Bk�BbNBe`BaHB_;B_;B\)B]/B[#BR�BQ�BP�BQ�BN�BQ�BS�BO�BI�BM�BI�B@�BF�BH�BF�BC�BD�BF�BA�B?}B;dB<jB9XB:^B;dB6FB7LB9XB8RB9XB6FB0!B�B�B&�B%�B �B#�B'�B%�B&�B)�B"�B�B%�B$�B�B�B�B�B�B#�B�B�B�B�B�B�BhB�B�B�B�B�B#�B$�B$�B!�B �B�B$�B,B'�B(�B&�B"�B�B#�B,B.B,B'�B$�B+B/B+B&�B33B0!B-B,B/B2-B49B6FB>wB=qB?}B>wB;dB?}BB�BF�BB�B>wB<jBG�BL�BM�BO�BS�BR�BR�BR�BP�BQ�BXB^5B_;B`BB]/BZBbNB^5B\)B_;BjBp�Bm�BjBk�Bk�Bs�By�By�B}�B~�B|�B�B�B~�Bz�B� B�B�=B�PB�\B�uB�hB�oB�oB�hB��B��B��B��B��B��B��B��B��B��B�B��B��B�B�B�3B�3B�jB��B�jBŢB��B�B��B��B��B�)B�;B�5B�;B�#B�NB�B�B�B�B�B�B�B�B��B	B	B	B	B	B	
=B	DB	
=B	bB	bB	hB	{B	�B	�B	�B	�B	�B	�B	!�B	$�B	$�B	'�B	(�B	'�B	)�B	)�B	+B	-B	2-B	5?B	8RB	9XB	9XB	9XB	<jB	?}B	>wB	=qB	B�B	G�B	G�B	G�B	F�B	E�B	D�B	L�B	ZB	\)B	YB	XB	_;B	bNB	cTB	dZB	ffB	hsB	m�B	m�B	n�B	o�B	r�B	r�B	s�B	w�B	� B	�B	�B	�B	�B	�B	�B	�1B	�=B	�\B	�bB	�bB	�oB	�oB	�hB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�-B	�3B	�-B	�-B	�-B	�3B	�9B	�?B	�LB	�LB	�RB	�RB	�RB	�LB	�LB	�LB	�FB	�LB	�FB	�9B	�3B	�FB	�^B	�wB	�}B	�qB	�wB	��B	��B	B	��B	��B	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�B	�#B	�#B	�#B	�5B	�5B	�/B	�B	�/B	�5B	�5B	�BB	�;B	�5B	�HB	�TB	�fB	�mB	�mB	�sB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
  B
  B
B
B
B
B
%B
%B
%B
B
B

=B

=B

=B
JB
JB
PB
JB
DB
PB
VB
bB
bB
bB
\B
\B
\B
\B
oB
uB
uB
oB
uB
oB
uB
uB
oB
oB
uB
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
 �B
�B
"�B
"�B
"�B
"�B
!�B
"�B
#�B
$�B
&�B
&�B
&�B
'�B
(�B
(�B
)�B
(�B
(�B
(�B
)�B
)�B
(�B
'�B
&�B
+B
-B
-B
.B
.B
.B
.B
.B
.B
-B
,B
,B
-B
,B
.B
/B
1'B
2-B
2-B
2-B
33B
33B
1'B
0!B
49B
5?B
5?B
49B
49B
2-B
49B
5?B
49B
49B
6FB
6FB
6FB
6FB
9XB
9XB
9XB
8RB
8RB
8RB
7LB
5?B
8RB
:^B
;dB
;dB
;dB
:^B
9XB
9XB
9XB
9XB
:^B
:^B
:^B
;dB
;dB
:^B
<jB
>wB
>wB
=qB
<jB
>wB
?}B
@�B
?}B
A�B
A�B
A�B
A�B
A�B
B�B
C�B
C�B
B�B
C�B
B�B
B�B
B�B
A�B
A�B
@�B
B�B
C�B
D�B
D�B
D�B
D�B
E�B
E�B
F�B
F�B
F�B
E�B
D�B
E�B
G�B
H�B
H�B
H�B
H�B
I�B
H�B
F�B
F�B
K�B
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
N�B
N�B
M�B
M�B
L�B
L�B
M�B
M�B
N�B
N�B
O�B
O�B
N�B
M�B
N�B
N�B
M�B
O�B
P�B
P�B
P�B
R�B
R�B
R�B
R�B
R�B
R�B
Q�B
S�B
R�B
R�B
R�B
Q�B
Q�B
T�B
VB
VB
S�B
R�B
W
B
W
B
XB
W
B
VB
T�B
VB
YB
YB
ZB
ZB
ZB
ZB
\)B
[#B
ZB
YB
[#B
\)B
]/B
]/B
]/B
_;B
_;B
^5B
]/B
_;B
^5B
]/B
\)B
[#B
_;B
`BB
_;B
`BB
`BB
_;B
aHB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
aHB
aHB
_;B
aHB
bNB
`BB
dZB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
e`B
e`B
ffB
gmB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
gmB
gmB
ffB
e`B
dZB
hsB
iyB
jB
iyB
hsB
gmB
iyB
iyB
jB
jB
jB
k�B
jB
k�B
k�B
k�B
l�B
k�B
jB
k�B
l�B
l�B
k�B
k�B
l�B
m�B
l�B
l�B
l�B
k�B
n�B
o�B
o�B
o�B
n�B
n�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
r�B
r�B
r�B
r�B
r�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�B�B�B��B�B�B�B�B�B�B�B�B��B�B�B��B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B� B��B�vB�]B�eBՁB�[B�B�.B�_B��B��B��Bb�B7�B%�BB�]B�B�VB�B�EB��B��B��Bb4Bc�BYKBI�B�B�BYB
�`B
�NB
�[B
��B
��B
�8B
�+B
��B
��B
��B
�vB
�iB
��B
�sB
�RB
��B
��B
��B
�1B
��B
|�B
y�B
s�B
o�B
e�B
eB
e�B
`�B
Z�B
S�B
LB
A�B
F%B
CGB
=�B
>�B
:�B
49B
(XB
&B
tB	��B	�dB	�B	ϫB	�B	�uB	�B	��B	�>B	��B	�>B	�B	�9B	�iB	��B	�}B	��B	�B	�B	��B	s�B	q[B	jB	l�B	n�B	lB	e�B	`'B	RoB	KxB	:*B	-B	OB	0�B	0�B	+�B	)�B	%�B	"�B	�B	5B	]B	WB	1B	�B	�B	B	�B	�B	B	'B��B�B��B�vB��BڠB�EB�lB��BΥB�oBϑB��BǮB��B�B� B�"B�)B�gB�ZB��B��B�CB��B��B�:B�_B��B�YB� B�yB��B�bB��B|Bi�B}�B��Bz*Bk�Bp�BvzBq�Bl�Bc�Bf�Bb�B`vB`vB]~B^5B\CBT{BS[BRTBSBO�BR�BT{BP�BJ�BNVBJ�BBBGzBIRBGzBD�BEmBGBB�B@iB<�B=<B:DB;0B<B7�B8B:B8�B9�B7B1B"B�B(
B'B"�B$�B(�B'B'�B*�B#�B/B&fB%�B �B �B�B�B�B$tB�B �B�B�BjB$B�B�B�B�B�B �B$tB%`B%zB"�B!�BB%zB,WB(�B)yB'�B#�B/B$�B,�B.}B,�B(�B%�B+�B/�B+�B($B3�B0�B-�B-B0B2�B5B6�B>�B=�B?�B>�B<B@BB�BF�BC-B?�B=�BHKBMPBNpBPbBTFBS[BSuBS�BQ�BR�BX�B^�B_�B`�B]�B[	Bb�B^�B]B`BBj�Bp�BnBkBl=Bl=BtBzBzDB~BBHB}VB�'B�AB�B{�B��B��B��B��B��B��B��B��B��B� B��B�B��B�B�/B�/B�HB�@B�nB�0B�=B�B��B�}B��B��B��B��B��B�"B�%B�BB�B�MBуB�{B�]B�pBބBߊB��B��B��B��B��B��B��B��B�B�cB�B	;B	3B	GB	{B	�B	
rB	xB	
�B	}B	�B	�B	�B	�B	�B	�B	B	�B	 B	!�B	%B	%B	(
B	)*B	($B	*0B	*0B	+6B	-wB	2aB	5ZB	8lB	9�B	9�B	9�B	<�B	?�B	>�B	=�B	B�B	G�B	G�B	G�B	F�B	E�B	E9B	MB	Z7B	\CB	YeB	X�B	_pB	b�B	cnB	dtB	ffB	hXB	m�B	m�B	n�B	o�B	r�B	r�B	tB	xB	� B	��B	�B	�3B	�3B	�[B	�SB	�KB	�rB	�vB	�}B	��B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	�B	�
B	�$B	�0B	�*B	�yB	�-B	�3B	�GB	�GB	�GB	�hB	�TB	�ZB	�LB	��B	�lB	��B	�RB	�fB	�fB	�fB	��B	��B	�`B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�"B	�B	�B	�"B	�B	�2B	�B	�,B	�,B	�$B	�EB	�EB	�1B	�1B	�7B	�7B	�=B	�WB	�=B	�OB	�jB	�dB	چB	�dB	�jB	�jB	�vB	�pB	ޞB	�|B	�B	�B	�B	�B	��B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�	B	�B	�B	�B	��B	��B	��B	�B	�"B	�B	�B	�B	�6B	�"B	�(B	�B	�B
 B
 4B
 B
 4B
;B
 OB
 iB
B
3B
GB
9B
?B
?B
YB
mB
�B

rB

rB

XB
dB
~B
�B
dB
xB
�B
�B
}B
�B
}B
�B
vB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
 �B
�B
#B
#B
#B
"�B
!�B
# B
$B
%B
'B
'B
'B
(
B
)B
)B
)�B
)B
)B
)B
*B
*B
)*B
(>B
'8B
+6B
-)B
-CB
.B
./B
.B
./B
./B
./B
-CB
,=B
,WB
-CB
,=B
.IB
/iB
1AB
2aB
2GB
2GB
3MB
3MB
1AB
0UB
49B
5?B
5ZB
4TB
4nB
2aB
4nB
5ZB
4TB
4nB
6zB
6zB
6zB
6�B
9XB
9XB
9rB
8�B
8�B
8�B
7fB
5�B
8�B
:xB
;B
;dB
;B
:�B
9rB
9�B
9�B
9�B
:xB
:�B
:�B
;�B
;B
:�B
<�B
>�B
>wB
=�B
<�B
>�B
?�B
@�B
?�B
A�B
A�B
A�B
A�B
A�B
B�B
C�B
C�B
B�B
C�B
B�B
B�B
B�B
A�B
A�B
@�B
B�B
C�B
D�B
D�B
D�B
D�B
E�B
E�B
F�B
F�B
F�B
E�B
D�B
E�B
G�B
H�B
H�B
H�B
H�B
I�B
H�B
F�B
F�B
K�B
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
N�B
N�B
NB
M�B
MB
L�B
M�B
NB
N�B
N�B
O�B
O�B
N�B
NB
N�B
N�B
N"B
O�B
Q B
Q B
QB
R�B
SB
R�B
SB
R�B
SB
R B
S�B
SB
SB
SB
R B
R B
T�B
VB
V9B
TFB
S@B
W
B
W$B
XB
W$B
VSB
U2B
V9B
Y1B
YKB
ZQB
ZQB
Z7B
Z7B
\CB
[WB
Z7B
YKB
[=B
\CB
]dB
]dB
]IB
_;B
_;B
^OB
]dB
_;B
^OB
]dB
\]B
[�B
_VB
`\B
_VB
`vB
`vB
_VB
abB
a|B
bNB
bNB
bhB
bhB
bhB
b�B
abB
abB
_�B
a|B
b�B
`vB
dtB
ffB
ffB
f�B
f�B
ffB
ffB
ffB
ffB
f�B
f�B
e�B
ezB
f�B
g�B
hsB
hsB
h�B
hsB
hsB
hsB
hsB
hsB
hsB
g�B
g�B
f�B
e�B
d�B
h�B
iyB
j�B
i�B
h�B
g�B
i�B
i�B
j�B
j�B
jB
k�B
j�B
k�B
k�B
k�B
l�B
k�B
j�B
k�B
l�B
l�B
k�B
k�B
l�B
m�B
l�B
l�B
l�B
k�B
n�B
o�B
o�B
o�B
n�B
n�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
r�B
r�B
r�B
r�B
r�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.07(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201805120036032018051200360320180512003603201806221241352018062212413520180622124135201806042120022018060421200220180604212002  JA  ARFMdecpA19c                                                                20180508093518  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180508003524  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180508003527  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180508003527  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180508003528  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180508003528  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180508003529  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180508003529  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180508003530  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180508003530                      G�O�G�O�G�O�                JA  ARUP                                                                        20180508005707                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180508153618  CV  JULD            G�O�G�O�F��                JM  ARCAJMQC2.0                                                                 20180511153603  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180511153603  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180604122002  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622034135  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115131516                      G�O�G�O�G�O�                