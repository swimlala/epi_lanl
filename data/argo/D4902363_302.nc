CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-11-19T00:36:21Z creation;2018-11-19T00:36:26Z conversion to V3.1;2019-12-19T07:27:44Z update;     
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
resolution        =���   axis      Z        |  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     |  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \t   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  `T   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �,   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  �h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     |  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  ̼   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �8   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �H   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �L   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �P   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �T   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �X   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181119003621  20200115131517  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              .A   JA  I2_0576_302                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @ؑ�E� 1   @ؑ�8� @8�� ѷ�d+��u��1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BXffB`  Bg��Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV�CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di�fDj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�C3DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�<�D�|�D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�3D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�\)@�@�A�HA>�HA^�HA~�HA�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�B�RB�RB�RB�RB'�RB/�RB7�RB?�RBG�RBO�RBX�B_�RBgQ�Bo�RBw�RB�RB��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CV�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
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
D {�D ��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D	{�D	��D
{�D
��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D {�D ��D!{�D!��D"{�D"��D#{�D#��D${�D$��D%{�D%��D&{�D&��D'{�D'��D({�D(��D){�D)��D*{�D*��D+{�D+��D,{�D,��D-{�D-��D.{�D.��D/{�D/��D0{�D0��D1{�D1��D2{�D2��D3{�D3��D4{�D4��D5{�D5��D6{�D6��D7{�D7��D8{�D8��D9{�D9��D:{�D:��D;{�D;��D<{�D<��D={�D=��D>{�D>��D?{�D?��D@{�D@��DA{�DA��DB{�DB��DC{�DC��DD{�DD��DE{�DE��DF{�DF��DG{�DG��DH{�DH��DI{�DI��DJ{�DJ��DK{�DK��DL{�DL��DM{�DM��DN{�DN��DO{�DO��DP{�DP��DQ{�DQ��DR{�DR��DS{�DS��DT{�DT��DU{�DU��DV{�DV��DW{�DW��DX{�DX��DY{�DY��DZ{�DZ��D[{�D[��D\{�D\��D]{�D]��D^{�D^��D_{�D_��D`{�D`��Da{�Da��Db{�Db��Dc{�Dc��Dd{�Dd��De{�De��Df{�Df��Dg{�Dg��Dh{�Dh��Di��Di��Dj{�Dj��Dk{�Dk��Dl{�Dl��Dm{�Dm��Dn{�Dn��Do{�Do��Dp{�Dp��Dq{�Dq��Dr{�Dr��Ds{�Ds��Dt{�Dt��Du{�Du��Dv{�Dv��Dw{�Dw��Dx{�Dx��Dy{�Dy��Dz{�Dz��D{{�D{��D|{�D|��D}{�D}��D~{�D~��D{�D��D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�:�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D½�D���D�=�D�}�Dý�D���D�=�D�}�DĽ�D���D�=�D�}�DŽ�D���D�=�D�}�Dƽ�D���D�=�D�}�Dǽ�D���D�@�D�}�DȽ�D���D�=�D�}�Dɽ�D���D�=�D�}�Dʽ�D���D�=�D�}�D˽�D���D�=�D�}�D̽�D���D�=�D�}�Dͽ�D���D�=�D�}�Dν�D���D�=�D�}�DϽ�D���D�=�D�}�Dн�D���D�=�D�}�Dѽ�D���D�=�D�}�Dҽ�D���D�=�D�}�Dӽ�D���D�=�D�}�DԽ�D���D�:�D�z�Dս�D���D�=�D�}�Dֽ�D���D�=�D�}�D׽�D���D�=�D�}�Dؽ�D���D�=�D�}�Dٽ�D� �D�=�D�}�Dڽ�D���D�=�D�}�D۽�D���D�=�D�}�Dܽ�D���D�=�D�}�Dݽ�D���D�=�D�}�D޽�D���D�=�D�}�D߽�D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D���D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�x�A�x�A�r�A�t�A�t�A�v�A�x�A�x�A�x�A�x�A�x�A�z�A�z�A�z�A�|�A�|�A�|�A�~�A�~�A�~�A�~�AˁAˁA�|�A�r�A��A��;A�\)A��`A�VA�33A�A���A�VA�A�A�C�A�VA��FA�=qA�C�A�|�A�G�A���A���A�  A��A�^5A�A�A�JA��#A��hA���A��RA�E�A��wA���A�M�A��/A���A��`A�G�A��A��jA��-A�C�A�ZA�r�A�x�A��A��RA�M�A���A���A��-A��A���A��FA�1'A��A�1'A�  A�`BA�1A�=qA�1A�ZA��#A�C�A��A�7LA��HA��A�z�A�=qA�n�A���A�A�A��9A�`BA�=qA�JA�(�A�A�A�A}��A|�HA{�
Az�yAz^5AyO�AxQ�Aw�-Av��At�ArJAqt�Aq+Ap�!Ap9XAop�An�Am�mAlAj9XAh�Af��Af�jAf��AedZAcƨAb�Aat�A_p�A^��A\ĜA\9XA\A�A\Q�A[��AZ�AY�7AX�RAW�-AVZAU�AU��AU/AT�AS�TAS�ARffAQƨAP��AP�uAO�FAN5?AM|�ALv�AJ��AIK�AH�AG�AGS�AG�AF�/AE�^AC��AB(�A@-A>�A>VA>{A=�A=|�A=�A<�+A<1A;`BA:�!A:I�A9��A8�jA7�;A7p�A7+A6n�A5��A4�HA3�A37LA2�+A1�
A0��A0~�A0A�A/�TA/&�A.VA-��A,z�A)��A'dZA%�A#��A#l�A#�A"$�A ��A�A�uAS�AjA��A��A��A$�A��AI�A�A+AA�jAM�A�A  A�A;dAI�A��AQ�A~�AA33A
bNA	��A	�A�9A��A�A�!Az�A��A&�A��AE�A`BA z�@�%@�v�@�{@�p�@��@�-@�?}@���@��@��u@�j@�@��T@�j@�!@��@�  @�|�@��#@�@��@�+@�R@�=q@�^@�/@��D@ߍP@�ff@���@�  @��@�&�@ӶF@�+@���@�$�@�bN@�@���@̴9@̣�@�Q�@�ƨ@�V@��@�`B@Å@�@�5?@���@��7@��@�r�@��
@��@��@�|�@���@�@��-@�X@�/@���@��D@�1'@�b@���@��w@�dZ@�J@�ƨ@��@�p�@�?}@��u@��@��m@���@��P@�K�@��!@�$�@��^@�7L@�A�@��@��j@��j@�1'@��@��@���@�`B@�Z@��m@�"�@��@��H@���@�ȴ@���@�E�@��^@��@��;@��@�l�@�J@��h@�`B@�Ĝ@�z�@��@��@��!@�ff@��#@���@�p�@���@�z�@�9X@�(�@���@�M�@���@��^@��7@�x�@�V@�r�@���@�33@�o@���@��@��\@�M�@�$�@�@��7@��@�x�@�p�@�Ĝ@��m@���@��@��@�v�@��^@�/@� �@��w@�C�@��@��@��R@�n�@�-@��T@��-@��h@��@�G�@��@���@���@���@�j@���@���@�
=@�V@�-@��@�{@�J@��@���@�/@��@��9@��D@��@�I�@�b@���@��@�ƨ@��@��R@��!@���@�v�@�M�@�5?@��@���@��T@��-@��@���@��u@�Z@�j@�j@�j@�j@��@��D@�(�@�@��@~��@~�+@~V@~5?@}��@}�@{��@{�@{t�@z�@z�!@z�@y��@yhs@x��@x  @w�@w;d@v��@vE�@vV@v$�@u@u�h@u?}@uV@uV@u`B@u��@u��@u��@u�@t��@t�@t�/@t�@s��@s��@s��@s�F@sdZ@r��@pbN@n��@nE�@n@m��@m��@mp�@m`B@m`B@mO�@mO�@l��@lZ@l�@k��@kt�@ko@j��@j��@j~�@j~�@i�@iX@i�@h�u@g�;@gl�@gK�@g;d@fȴ@eO�@c�
@c@b=q@a�^@ahs@a&�@`�9@` �@_+@^�+@^$�@]�-@]?}@]V@\��@\z�@\I�@\9X@\�@[�m@[��@[S�@Z�@Zn�@Y�^@Yhs@YG�@Y7L@Y7L@X��@W�w@V��@Vff@V$�@U��@U�-@Up�@U/@U�@T�/@T�@T1@Sƨ@S�F@S�@SS�@S33@R�H@R�\@Q��@Qhs@PA�@O\)@N��@N�R@Nv�@NV@NE�@N$�@N@M�@M��@M��@M/@L��@L�@Lz�@K��@KS�@Ko@J�\@I�@I�^@I��@I��@I�7@IG�@H��@HbN@H �@G�w@G|�@GK�@G�@F�R@Fv�@FV@FE�@F{@E�@E�T@E@E�-@E�h@EO�@E/@EV@D�@D�j@D�D@DZ@D�@C�
@C�F@Ct�@C33@C@B�@B�H@B��@B�!@B��@B~�@Bn�@BM�@B=q@B=q@BJ@A�7@@��@@Ĝ@@�9@@�9@@�@@1'@@1'@@ �@@ �@@  @?�@>$�@>{@>@=�@=�T@=�-@=O�@<��@<�/@<��@<��@<�j@<�j@<�j@<�j@<�@<��@<�D@<j@<Z@<I�@<(�@<�@;��@;��@;�m@;�@:�@9x�@97L@8��@8bN@8  @7�;@7��@7��@7\)@7+@7
=@6��@6��@6��@6�@6ȴ@6�+@6$�@5��@5�-@5��@5p�@4�@4z�@3��@3o@2�\@2��@2�!@2�!@2J@1G�@17L@1%@0��@0Ĝ@0��@0Ĝ@0Q�@/�w@/;d@.��@.�+@.V@.5?@.{@-�@-@-�h@-O�@-�@,�/@,�j@,��@,I�@,(�@,�@+dZ@*�@*^5@)�#@)��@)7L@(Ĝ@'�@'�@'|�@'|�@'K�@&�@&��@&��@&�+@&E�@&{@%�@%�T@%��@%�@%V@$�@#��@#��@#"�@"��@"��@"�!@"��@"^5@"J@!hs@ ��@ ��@ �9@ ��@ b@�@|�@l�@l�@\)@\)@K�@;d@+@�@
=@
=@�y@v�@ff@V@V@5?@{@�@��@�-@`B@�@�/@��@z�@z�@Z@1@�F@��@S�@C�@�@��@�@�^@x�@&�@�@�@�@�`@Ĝ@��@�@bN@ �@\)@�@��@E�@5?@$�@$�@@@��@�-@�-@�@`B@`B@`B@O�@O�@O�@?}@V@�/@�@�D@I�@��@��@�@�@t�@C�@�@��@�\@^5@��@&�@��@Ĝ@�9@�9@�9@��@�u@r�@Q�@A�@1'@ �@�;@�@�@�@�@�@�@l�@�@��@V@�@�T@��@��@@�-@�h@O�@�@�@�@z�@j@Z@Z@(�@��@�
@ƨ@�F@��@�@t�@S�@S�@S�@33@"�@o@@@
�@
�!@
�\@
M�@	��@	��@	�7@	X@��@�u@�@A�@�@��@|�@|�@|�@l�@+@�@
=@
=@�y@�@�@�@ȴ@��@v�@E�@��@@�-@��@`B@?}@��@�/@�@�D@9X@�m@��@33@��@n�@^5@M�@M�@=q@�@J@�@�#@��@��@��@�^@�^@��@��@x�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�x�A�x�A�r�A�t�A�t�A�v�A�x�A�x�A�x�A�x�A�x�A�z�A�z�A�z�A�|�A�|�A�|�A�~�A�~�A�~�A�~�AˁAˁA�|�A�r�A��A��;A�\)A��`A�VA�33A�A���A�VA�A�A�C�A�VA��FA�=qA�C�A�|�A�G�A���A���A�  A��A�^5A�A�A�JA��#A��hA���A��RA�E�A��wA���A�M�A��/A���A��`A�G�A��A��jA��-A�C�A�ZA�r�A�x�A��A��RA�M�A���A���A��-A��A���A��FA�1'A��A�1'A�  A�`BA�1A�=qA�1A�ZA��#A�C�A��A�7LA��HA��A�z�A�=qA�n�A���A�A�A��9A�`BA�=qA�JA�(�A�A�A�A}��A|�HA{�
Az�yAz^5AyO�AxQ�Aw�-Av��At�ArJAqt�Aq+Ap�!Ap9XAop�An�Am�mAlAj9XAh�Af��Af�jAf��AedZAcƨAb�Aat�A_p�A^��A\ĜA\9XA\A�A\Q�A[��AZ�AY�7AX�RAW�-AVZAU�AU��AU/AT�AS�TAS�ARffAQƨAP��AP�uAO�FAN5?AM|�ALv�AJ��AIK�AH�AG�AGS�AG�AF�/AE�^AC��AB(�A@-A>�A>VA>{A=�A=|�A=�A<�+A<1A;`BA:�!A:I�A9��A8�jA7�;A7p�A7+A6n�A5��A4�HA3�A37LA2�+A1�
A0��A0~�A0A�A/�TA/&�A.VA-��A,z�A)��A'dZA%�A#��A#l�A#�A"$�A ��A�A�uAS�AjA��A��A��A$�A��AI�A�A+AA�jAM�A�A  A�A;dAI�A��AQ�A~�AA33A
bNA	��A	�A�9A��A�A�!Az�A��A&�A��AE�A`BA z�@�%@�v�@�{@�p�@��@�-@�?}@���@��@��u@�j@�@��T@�j@�!@��@�  @�|�@��#@�@��@�+@�R@�=q@�^@�/@��D@ߍP@�ff@���@�  @��@�&�@ӶF@�+@���@�$�@�bN@�@���@̴9@̣�@�Q�@�ƨ@�V@��@�`B@Å@�@�5?@���@��7@��@�r�@��
@��@��@�|�@���@�@��-@�X@�/@���@��D@�1'@�b@���@��w@�dZ@�J@�ƨ@��@�p�@�?}@��u@��@��m@���@��P@�K�@��!@�$�@��^@�7L@�A�@��@��j@��j@�1'@��@��@���@�`B@�Z@��m@�"�@��@��H@���@�ȴ@���@�E�@��^@��@��;@��@�l�@�J@��h@�`B@�Ĝ@�z�@��@��@��!@�ff@��#@���@�p�@���@�z�@�9X@�(�@���@�M�@���@��^@��7@�x�@�V@�r�@���@�33@�o@���@��@��\@�M�@�$�@�@��7@��@�x�@�p�@�Ĝ@��m@���@��@��@�v�@��^@�/@� �@��w@�C�@��@��@��R@�n�@�-@��T@��-@��h@��@�G�@��@���@���@���@�j@���@���@�
=@�V@�-@��@�{@�J@��@���@�/@��@��9@��D@��@�I�@�b@���@��@�ƨ@��@��R@��!@���@�v�@�M�@�5?@��@���@��T@��-@��@���@��u@�Z@�j@�j@�j@�j@��@��D@�(�@�@��@~��@~�+@~V@~5?@}��@}�@{��@{�@{t�@z�@z�!@z�@y��@yhs@x��@x  @w�@w;d@v��@vE�@vV@v$�@u@u�h@u?}@uV@uV@u`B@u��@u��@u��@u�@t��@t�@t�/@t�@s��@s��@s��@s�F@sdZ@r��@pbN@n��@nE�@n@m��@m��@mp�@m`B@m`B@mO�@mO�@l��@lZ@l�@k��@kt�@ko@j��@j��@j~�@j~�@i�@iX@i�@h�u@g�;@gl�@gK�@g;d@fȴ@eO�@c�
@c@b=q@a�^@ahs@a&�@`�9@` �@_+@^�+@^$�@]�-@]?}@]V@\��@\z�@\I�@\9X@\�@[�m@[��@[S�@Z�@Zn�@Y�^@Yhs@YG�@Y7L@Y7L@X��@W�w@V��@Vff@V$�@U��@U�-@Up�@U/@U�@T�/@T�@T1@Sƨ@S�F@S�@SS�@S33@R�H@R�\@Q��@Qhs@PA�@O\)@N��@N�R@Nv�@NV@NE�@N$�@N@M�@M��@M��@M/@L��@L�@Lz�@K��@KS�@Ko@J�\@I�@I�^@I��@I��@I�7@IG�@H��@HbN@H �@G�w@G|�@GK�@G�@F�R@Fv�@FV@FE�@F{@E�@E�T@E@E�-@E�h@EO�@E/@EV@D�@D�j@D�D@DZ@D�@C�
@C�F@Ct�@C33@C@B�@B�H@B��@B�!@B��@B~�@Bn�@BM�@B=q@B=q@BJ@A�7@@��@@Ĝ@@�9@@�9@@�@@1'@@1'@@ �@@ �@@  @?�@>$�@>{@>@=�@=�T@=�-@=O�@<��@<�/@<��@<��@<�j@<�j@<�j@<�j@<�@<��@<�D@<j@<Z@<I�@<(�@<�@;��@;��@;�m@;�@:�@9x�@97L@8��@8bN@8  @7�;@7��@7��@7\)@7+@7
=@6��@6��@6��@6�@6ȴ@6�+@6$�@5��@5�-@5��@5p�@4�@4z�@3��@3o@2�\@2��@2�!@2�!@2J@1G�@17L@1%@0��@0Ĝ@0��@0Ĝ@0Q�@/�w@/;d@.��@.�+@.V@.5?@.{@-�@-@-�h@-O�@-�@,�/@,�j@,��@,I�@,(�@,�@+dZ@*�@*^5@)�#@)��@)7L@(Ĝ@'�@'�@'|�@'|�@'K�@&�@&��@&��@&�+@&E�@&{@%�@%�T@%��@%�@%V@$�@#��@#��@#"�@"��@"��@"�!@"��@"^5@"J@!hs@ ��@ ��@ �9@ ��@ b@�@|�@l�@l�@\)@\)@K�@;d@+@�@
=@
=@�y@v�@ff@V@V@5?@{@�@��@�-@`B@�@�/@��@z�@z�@Z@1@�F@��@S�@C�@�@��@�@�^@x�@&�@�@�@�@�`@Ĝ@��@�@bN@ �@\)@�@��@E�@5?@$�@$�@@@��@�-@�-@�@`B@`B@`B@O�@O�@O�@?}@V@�/@�@�D@I�@��@��@�@�@t�@C�@�@��@�\@^5@��@&�@��@Ĝ@�9@�9@�9@��@�u@r�@Q�@A�@1'@ �@�;@�@�@�@�@�@�@l�@�@��@V@�@�T@��@��@@�-@�h@O�@�@�@�@z�@j@Z@Z@(�@��@�
@ƨ@�F@��@�@t�@S�@S�@S�@33@"�@o@@@
�@
�!@
�\@
M�@	��@	��@	�7@	X@��@�u@�@A�@�@��@|�@|�@|�@l�@+@�@
=@
=@�y@�@�@�@ȴ@��@v�@E�@��@@�-@��@`B@?}@��@�/@�@�D@9X@�m@��@33@��@n�@^5@M�@M�@=q@�@J@�@�#@��@��@��@�^@�^@��@��@x�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�'B�B0!BD�BD�B?}B:^B�{B��B��B�B�B��B�uB�VB��B�1BZBe`B�\B�bB�1B~�B|�B�Bk�BG�BhsB]/BJ�BA�B1'B5?B1'B.B�B#�B'�B,B"�B\BJBB�B�sB�/B�B�}B�wB�-B��B�hB�VB�7B�%B{�Bm�Bs�BjBbNBZBYBN�BF�B@�B<jB2-B+B�B
��B
�mB
��B
�'B
��B
��B
�B
��B
��B
�PB
s�B
cTB
^5B
\)B
T�B
O�B
L�B
B�B
7LB
49B
&�B
hB
%B
uB
�B
uB
bB
1B
B	��B	�B	�HB	�5B	��B	�#B	�
B	ȴB	�wB	�XB	�'B	��B	��B	�hB	��B	�B	��B	��B	��B	�VB	�DB	�B	w�B	|�B	z�B	{�B	q�B	w�B	v�B	o�B	m�B	hsB	jB	bNB	VB	VB	L�B	E�B	9XB	?}B	;dB	<jB	:^B	33B	#�B	VB		7B��B��B��B	  B	B��B��B��B��B��B�B�B�B�`B�HB�HB�5B�B��B��BĜBÖB�wB�wB�RB�^B�jB�FB�B��B��B��B�B}�B�B� B�%B�1B}�By�Bp�Br�BiyBl�BjBcTB[#BW
BT�B\)BaHBZBaHB_;B\)B\)BW
BH�B8RBF�BC�B:^B-BA�B<jB6FB8RB:^B;dB5?B2-B=qB:^B33B+B$�B�B%�B"�B�B�B.B+B%�B�B+B-B0!B.B+B#�B�B�B�BhB(�B"�B�B\B�B�B'�B&�B%�B&�B!�B�B�B�B{BhBB�B'�B(�B$�B�B�B%�B/B.B+B#�B�B�B#�B%�B0!B:^B<jB?}B?}B>wB>wB=qB9XB@�BJ�BI�BN�BN�BP�BO�BM�BN�BO�BO�BM�BI�BC�B>wBH�BYB\)BZB[#B_;B`BB_;B_;B^5B]/B_;B]/B[#B]/B_;Bp�Bl�Bk�Bz�Bw�Br�Bu�B~�B� B�+B�7B�7B�7B�+B�B�B� B�+B�hB�JB�%B�JB�hB�VB�oB�bB��B��B��B��B��B��B��B��B��B��B��B��B�3B�?B�FB�LB�9B�9B�FB�jBBÖBÖBÖBĜBŢBŢB��B��B��B��BȴB��B�B�B�B�B�#B�BB�NB�B�B�B�B�B��B��B��B��B��B��B��B��B	B	B	B	  B	B	%B	1B	\B	�B	�B	�B	�B	�B	�B	"�B	+B	0!B	49B	5?B	7LB	9XB	=qB	=qB	>wB	>wB	F�B	O�B	O�B	P�B	R�B	T�B	VB	VB	VB	VB	VB	[#B	bNB	hsB	m�B	m�B	n�B	n�B	q�B	s�B	t�B	x�B	}�B	� B	�B	�B	�1B	�1B	�1B	�7B	�PB	�bB	�VB	�\B	�hB	�oB	�{B	�oB	�hB	�uB	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�'B	�!B	�-B	�?B	�FB	�FB	�LB	�RB	�?B	�dB	��B	ŢB	ŢB	ƨB	ƨB	ȴB	ȴB	ȴB	ǮB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	��B	��B	��B	�)B	�5B	�HB	�TB	�ZB	�TB	�TB	�ZB	�fB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
%B
B
%B
%B
%B
B
B
B
+B
%B
B
B
	7B
+B
1B
JB
PB
PB
PB
JB
DB
PB
\B
VB
bB
hB
bB
bB
oB
uB
uB
uB
{B
�B
�B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
$�B
$�B
%�B
%�B
$�B
%�B
%�B
%�B
$�B
"�B
!�B
�B
&�B
&�B
&�B
'�B
)�B
+B
)�B
)�B
+B
,B
,B
,B
,B
+B
+B
)�B
)�B
)�B
,B
+B
)�B
&�B
(�B
'�B
&�B
)�B
/B
/B
-B
+B
+B
1'B
1'B
1'B
2-B
2-B
1'B
/B
.B
/B
1'B
1'B
2-B
33B
33B
33B
33B
33B
33B
49B
49B
5?B
5?B
49B
5?B
5?B
2-B
33B
5?B
5?B
7LB
6FB
6FB
5?B
:^B
;dB
<jB
;dB
:^B
<jB
>wB
>wB
=qB
>wB
>wB
>wB
>wB
=qB
;dB
<jB
;dB
>wB
?}B
@�B
B�B
B�B
B�B
@�B
@�B
?}B
A�B
D�B
D�B
D�B
B�B
C�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
F�B
E�B
H�B
H�B
I�B
H�B
H�B
H�B
H�B
H�B
G�B
H�B
I�B
J�B
L�B
L�B
K�B
K�B
K�B
N�B
M�B
N�B
M�B
M�B
M�B
O�B
Q�B
R�B
T�B
T�B
T�B
S�B
S�B
T�B
T�B
S�B
R�B
Q�B
S�B
T�B
VB
YB
YB
YB
YB
YB
ZB
\)B
\)B
[#B
[#B
\)B
]/B
\)B
\)B
\)B
\)B
[#B
[#B
[#B
\)B
[#B
[#B
\)B
^5B
_;B
^5B
]/B
]/B
^5B
^5B
^5B
]/B
\)B
`BB
cTB
cTB
cTB
cTB
cTB
cTB
bNB
cTB
dZB
e`B
dZB
dZB
e`B
ffB
ffB
ffB
ffB
e`B
dZB
dZB
cTB
e`B
e`B
gmB
hsB
hsB
hsB
gmB
gmB
ffB
gmB
hsB
hsB
iyB
jB
jB
jB
iyB
iyB
jB
k�B
k�B
jB
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
k�B
jB
jB
jB
iyB
l�B
k�B
k�B
jB
l�B
m�B
l�B
m�B
m�B
o�B
p�B
p�B
o�B
o�B
p�B
q�B
p�B
p�B
p�B
q�B
q�B
p�B
p�B
p�B
p�B
p�B
r�B
r�B
r�B
q�B
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
t�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
w�B
w�B
x�B
x�B
x�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B�
B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B��B��B0�BEmBF�BC{BAB��B�WB��B��B��B��B��B��B�]B��Ba�Bi�B�B�4B��B��B~�B�Bn�BLJBiDB_pBM6BD�B4nB6�B2�B/�B"hB%`B)*B,�B$B�B�BMB��B�B�pB�_B�[B�B�B��B�,B�bB�^B��B~BBoOBtTBk�BcnB[�BY�BPbBG�BA�B=�B3�B,WB�B
��B
��B
�B
�ZB
��B
�QB
��B
��B
��B
��B
v`B
f�B
`'B
]~B
VmB
Q B
M�B
C�B
8�B
5?B
(�B
,B
�B
,B
B
,B
B
	7B
-B	�JB	��B	�nB	�B	��B	�qB	׍B	�XB	�iB	��B	��B	�B	�B	��B	�@B	�B	�0B	��B	��B	�B	�dB	��B	yrB	}�B	{B	|�B	r�B	xRB	w�B	p�B	n}B	i�B	kB	c�B	W�B	W$B	NVB	G�B	;dB	@�B	<jB	=B	:�B	4B	%�B	4B	DB�VB�>B��B	 iB	uB��B��B��B��B��B�B�hB�B�B�hB��B��B�1B�B�B��BĶB�}B�}B�rB�B��B�B�UB�0B�$B��B�zB� B�B�AB��B�B}B{Br|Bt9Bk6Bm�Bk�Bd�B]/BX�BV�B]/Ba�B[=Ba�B_�B\�B\�BW�BJXB:�BG�BD�B<�B/�BB[B=�B7�B9XB;0B<B6`B3�B=�B:�B4TB,qB&�B�B'RB$tBB)B.}B+�B&�B B+�B-wB0oB.IB+kB$�B�B�B�B&B)*B#�B�B4BB�B(>B'mB&fB'mB"�B �B�B �B�B&B+B~B(sB)yB%�B;B)B&�B/OB.IB+�B$�B�BCB$�B'B0�B:�B<�B?�B@ B?B?.B>]B:�BAoBKDBJ�BO(BO(BQBPHBN<BO(BP.BP.BN<BJrBD�B@4BI�BYeB\xBZ�B[�B_pB`vB_�B_�B^�B]�B_�B]�B\)B^�B`\Bp�BmCBl�Bz�Bx8Bs�BvzBcB��B�_B�RB�RB�RB�_B��B��B�B��B��B��B�EB��B��B��B��B�4B��B��B��B�B�B�4B�NB�8B�DB�DB��B��B�hB�tB��B��B��B��B��B��B��B��B��B��B��B��B��B�B��B��B�B�lB�^B�SB�SBևBٚB��B��B�B��B� B��B�B�B��B�B�*B�B�B�B�(B�BB	B	-B	;B	 iB	uB	�B	�B	�B	�B	�B	�B	�B	�B	B	#:B	+QB	0UB	4TB	5ZB	7�B	9�B	=�B	=�B	>�B	?B	F�B	O�B	O�B	QB	SB	UB	V9B	V9B	V9B	VSB	V�B	[qB	bhB	h�B	m�B	m�B	n�B	n�B	q�B	s�B	uB	y$B	~(B	�4B	�[B	�MB	�fB	�fB	��B	��B	��B	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	��B	��B	�B	�/B	�/B	�CB	�;B	�AB	��B	�aB	�ZB	�`B	�`B	��B	��B	�+B	��B	��B	��B	żB	��B	��B	��B	ȴB	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	�B	�B	�B	�B	� B	�@B	�SB	�+B	�?B	�gB	҉B	՛B	�xB	ބB	�|B	�nB	�tB	�B	�B	��B	�B	�B	��B	�B	��B	��B	��B	�B	�B	�B	�B	�B	��B	��B	��B	� B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�	B	�	B	��B	�B	��B	��B	�	B	�B	�B	�B	�B	�B	�B	�B	�$B	�>B	�>B	�6B
'B
-B
MB
9B
YB
9B
?B
?B
?B
9B
SB
SB
EB
?B
SB
mB
	RB
_B
�B
~B
jB
PB
jB
~B
�B
�B
vB
�B
�B
�B
}B
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
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
$�B
$�B
%�B
%�B
$�B
%�B
%�B
%�B
$�B
#B
"B
;B
&�B
'B
'8B
($B
*B
+B
*B
*0B
+B
,B
,B
,B
,B
+B
+B
*B
*0B
*0B
,"B
+B
*0B
'RB
)*B
($B
'8B
*0B
/5B
/5B
-)B
+QB
+QB
1AB
1AB
1[B
2GB
2-B
1AB
/OB
.IB
/OB
1[B
1[B
2GB
3hB
3hB
3MB
3MB
3MB
3MB
4TB
4TB
5tB
5ZB
4TB
5tB
5tB
2|B
3�B
5tB
5tB
7�B
6zB
6�B
5�B
:�B
;B
<jB
;B
:�B
<�B
>wB
>wB
=�B
>�B
>�B
>�B
>�B
=�B
;�B
<�B
;�B
>�B
?�B
@�B
B�B
B�B
B�B
@�B
@�B
?�B
A�B
D�B
D�B
D�B
B�B
C�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
F�B
E�B
H�B
H�B
I�B
H�B
H�B
H�B
H�B
H�B
G�B
H�B
I�B
J�B
L�B
L�B
K�B
K�B
K�B
N�B
NB
N�B
M�B
NB
N"B
O�B
R B
SB
T�B
UB
UB
T,B
TB
U2B
U2B
TB
S@B
R:B
T,B
U2B
VB
YB
Y1B
YB
Y1B
Y1B
ZQB
\CB
\CB
[WB
[WB
\)B
]IB
\)B
\)B
\CB
\CB
[=B
[=B
[=B
\CB
[=B
[=B
\CB
^5B
_VB
^jB
]IB
]IB
^OB
^OB
^OB
]~B
\xB
`\B
cTB
cnB
cTB
cTB
cTB
cnB
bhB
cnB
dZB
ezB
d�B
dtB
e�B
ffB
ffB
ffB
ffB
e`B
dtB
d�B
c�B
e�B
ezB
gmB
h�B
hsB
hsB
g�B
g�B
f�B
g�B
h�B
h�B
i�B
jB
jB
j�B
i�B
i�B
j�B
k�B
k�B
j�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
k�B
j�B
j�B
j�B
i�B
l�B
k�B
k�B
j�B
l�B
m�B
l�B
m�B
m�B
o�B
p�B
p�B
o�B
o�B
p�B
q�B
p�B
p�B
p�B
q�B
q�B
p�B
p�B
p�B
p�B
p�B
r�B
r�B
r�B
q�B
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
t�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
w�B
w�B
x�B
x�B
x�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.07(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201811230036002018112300360020181123003600201811230200162018112302001620181123020016201811240024582018112400245820181124002458  JA  ARFMdecpA19c                                                                20181119093620  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20181119003621  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20181119003625  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20181119003625  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20181119003626  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20181119003626  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20181119003626  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20181119003626  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20181119003626  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20181119003626                      G�O�G�O�G�O�                JA  ARUP                                                                        20181119005629                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20181119153603  CV  JULD            G�O�G�O�Fč�                JM  ARGQJMQC2.0                                                                 20181119153603  CV  JULD_LOCATION   G�O�G�O�Fč�                JM  ARGQJMQC2.0                                                                 20181119153603  CV  LATITUDE        G�O�G�O�AǑh                JM  ARCAJMQC2.0                                                                 20181122153600  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20181122153600  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20181122170016  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20181123152458  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115131517                      G�O�G�O�G�O�                