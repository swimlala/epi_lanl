CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-06-10T00:35:04Z creation;2018-06-10T00:35:08Z conversion to V3.1;2019-12-19T07:40:18Z update;     
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
_FillValue                    �LArgo profile    3.1 1.2 19500101000000  20180610003504  20200115131518  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0576_248                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�i5��� 1   @�i6�W @:�C���d?��rG1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CA�fCD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=fD=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dg��Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D���D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D��3D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D��f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�@���@���Az�A<z�A\z�A|z�A�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qB�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\BÏ\BǏ\Bˏ\BϏ\Bӏ\B׏\Bۏ\Bߏ\B�\B�\B�\B�\B�\B��\B��\B��\CǮCǮCǮCǮC	ǮCǮCǮCǮCǮCǮCǮCǮCǮCǮCǮCǮC!ǮC#ǮC%ǮC'ǮC)ǮC+ǮC-ǮC/ǮC1ǮC3ǮC5ǮC7ǮC9ǮC;ǮC=ǮC?ǮCA�CCǮCEǮCGǮCIǮCKǮCMǮCOǮCQǮCSǮCUǮCWǮCYǮC[ǮC]ǮC_ǮCaǮCcǮCeǮCgǮCiǮCkǮCmǮCoǮCqǮCsǮCuǮCwǮCyǮC{ǮC}ǮCǮC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��
C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D q�D ��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��DxRD��D	q�D	��D
q�D
��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��D q�D ��D!q�D!��D"q�D"��D#q�D#��D$q�D$��D%q�D%��D&q�D&��D'q�D'��D(q�D(��D)q�D)��D*q�D*��D+q�D+��D,q�D,��D-q�D-��D.q�D.��D/q�D/��D0q�D0��D1q�D1��D2q�D2��D3q�D3��D4q�D4��D5q�D5��D6q�D6��D7q�D7��D8q�D8��D9q�D9��D:q�D:��D;q�D;��D<q�D<�RD=q�D=��D>q�D>��D?q�D?��D@q�D@��DAq�DA��DBq�DB��DCq�DC��DDq�DD��DEq�DE��DFq�DF��DGq�DG��DHq�DH��DIq�DI��DJq�DJ��DKq�DK��DLq�DL��DMq�DM��DNq�DN��DOq�DO��DPq�DP��DQq�DQ��DRq�DR��DSq�DS��DTq�DT��DUq�DU��DVq�DV��DWq�DW��DXq�DX��DYq�DY��DZq�DZ��D[q�D[��D\q�D\��D]q�D]��D^q�D^��D_q�D_��D`q�D`��Daq�Da��Dbq�Db��Dcq�Dc��Ddq�Dd��Deq�De��Dfq�Df��Dgq�Dg�Dhq�Dh��Diq�Di��Djq�Dj��Dkq�Dk��Dlq�Dl��Dmq�Dm��Dnq�Dn��Doq�Do��Dpq�Dp��Dqq�Dq��Drq�Dr��Dsq�Ds��Dtq�Dt��Duq�Du��Dvq�Dv��Dwq�Dw��Dxq�Dx��Dyq�Dy��Dzq�Dz��D{q�D{��D|q�D|��D}q�D}��D~q�D~��Dq�D��D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D¸�D���D�8�D�x�Dø�D���D�8�D�x�Dĸ�D���D�8�D�x�DŸ�D���D�8�D�x�DƸ�D���D�8�D�x�DǸ�D���D�8�D�x�Dȸ�D���D�8�D�x�Dɸ�D���D�8�D�x�Dʸ�D���D�8�D�x�D˸�D���D�8�D�x�D̼)D���D�8�D�x�D͸�D���D�8�D�x�Dθ�D���D�8�D�x�Dϸ�D���D�8�D�x�Dи�D���D�8�D�x�DѸ�D���D�8�D�x�DҸ�D���D�8�D�x�DӸ�D���D�8�D�x�DԸ�D���D�8�D�x�Dո�D���D�8�D�x�Dָ�D���D�8�D�x�D׸�D���D�8�D�x�Dظ�D���D�8�D�x�Dٸ�D���D�8�D�x�Dڸ�D���D�8�D�x�D۸�D���D�8�D�x�Dܸ�D���D�8�D�x�Dݸ�D���D�8�D�x�D޸�D���D�8�D�x�D߸�D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�<)D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D���D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�|)D��\1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��wA��-A�x�A�VA�33A��A���A��HA���A��9A��hA�/A��A��A�x�A�7LA��9A�ZA��TA�A���A�JA��A�l�A���A�A�/A�~�A�O�A�VA��A���A��A�hsA��A�S�A�
=A���A�ffA�bNA��A��DA�+A�1A�=qA�\)A��
A�A�~�A���A�ZA��HA� �A�jA�p�A��A��uA��A���A���A�~�A�jA�O�A�1'A���A�ƨA�l�A���A�I�A�  A��A���A��A�VA��wA�ĜA�A~�!A}�TA}�7A}/A|��A|AzI�Av��As��Asl�Ar�HAq�TAq�ApbNAo��An��Am��Ak�AkVAjA�AiAg�Ag|�AgVAfVAd�+Ac�#Ac|�Ab�\A`ĜA_�A^z�A^E�A]|�A[K�AZ{AX��AV�!AU��AU�AT�ASVARr�AP�`AO�AM|�AKAI��AH��AG`BAFE�AF  AC�^AB�HABr�AB5?AB  AA�A@�yA@��A@n�A@ �A?�A=��A<VA;�A;��A;&�A8�uA7��A7��A7|�A7l�A7oA6��A6��A5�A4��A3K�A2^5A1A1G�A0�A0z�A01'A0{A/�wA/hsA.JA-�PA,�yA*��A(ffA'33A%�A#�A#�A!�A!/A 1'A�A��A�TAhsAn�A��A1'A�AA�\An�A(�A��A��A�A7LA�A�RA/A5?A�mA�A��AJA��A�hAVA�A�A=qA�hAVA	ƨA��A�A�7A~�A�#At�A�RA9XAƨAG�A�;A �A 9X@�ƨ@�ȴ@��-@�Z@��y@�V@��@��@�r�@�/@�Z@�(�@�1@�ƨ@�dZ@�v�@�1@�{@�V@�j@�9X@�dZ@�E�@�x�@���@�@㝲@��T@�&�@�Ĝ@�bN@���@��@��@�A�@ץ�@�S�@ָR@���@թ�@�p�@�Ĝ@� �@Ӯ@���@Ѻ^@��/@���@��y@�X@̛�@�r�@�  @�K�@�M�@�@ə�@�`B@��/@�9X@ǶF@ǝ�@�l�@�"�@Ə\@�E�@���@��;@+@���@��@�ƨ@���@�l�@�K�@�"�@��@���@�M�@���@�Z@��y@�J@�p�@��u@��@��!@�p�@��j@�1@���@���@�n�@��-@�Z@���@��@��H@��R@�{@���@�p�@���@���@�"�@�v�@���@��@�o@�{@��h@�7L@�1'@��P@�\)@���@��@�r�@���@�@��!@�v�@�=q@�{@���@��@���@�A�@��w@��H@�M�@�-@�@�p�@���@�Ĝ@���@�@���@�V@���@�9X@��
@��P@�S�@�o@��R@�M�@��@�x�@�&�@��`@��D@�Q�@��m@�+@��H@�ȴ@��R@�~�@�V@�M�@���@�Ĝ@��u@���@���@��u@�r�@�1'@��m@��@��@���@�n�@�-@���@��9@�"�@�^5@��@���@���@���@��h@�p�@�`B@��@���@�Q�@� �@���@��
@�ƨ@��@�K�@�
=@��R@��\@�M�@�-@��@�@�hs@��@��`@���@��D@��@�A�@�w@K�@~��@~�R@~V@~$�@}�@}��@}�-@}��@}��@}�@}`B@}?}@|�@|�j@|j@|(�@{��@z�\@yx�@x��@xĜ@x�9@x�u@xbN@xQ�@xA�@xA�@xA�@x �@xb@w�@w��@w�w@w��@w�P@wl�@w\)@w�@v�@vV@u�T@u/@t��@tI�@sdZ@r��@r-@q�@q��@q�7@qX@q7L@q�@p��@p�`@p�9@pbN@o�;@o��@o\)@ol�@o|�@oK�@m��@l�@lj@k��@kƨ@kS�@kC�@ko@j�!@j~�@jM�@i�@ix�@i%@h�`@hĜ@h�9@h��@h��@h�u@h�@h�@hr�@hbN@hQ�@hQ�@hA�@hA�@h1'@hb@g�@g�w@g��@gl�@f�R@f@e`B@d�j@dI�@d�@c��@c@b��@b~�@bM�@a��@a��@a7L@` �@_�@_��@_�@_l�@_;d@_
=@^�R@^V@]�T@]��@]p�@\z�@\1@[��@[�m@[�m@[�m@[ƨ@[��@[dZ@[C�@[33@[@Z��@Zn�@Z-@Y��@Y�@W�;@Vȴ@Vȴ@Vȴ@V�R@V��@V�+@V@U�-@T�j@S�
@S�F@St�@R�@R�\@R^5@R-@RJ@Q�@Q�#@Q&�@P�`@PĜ@PĜ@P��@P�u@P�@P�@P�@P�@Pr�@P�@P�@P�@P�@P �@Nȴ@M�@MO�@M?}@M�@L�@LZ@LI�@LZ@LI�@L(�@K��@K�F@K��@KS�@K@J�!@JJ@IX@H�9@Hr�@Hb@G�;@G�@G�P@G|�@F��@E��@EO�@D�j@C��@C�m@C��@Ct�@CS�@CS�@CC�@CC�@CC�@C"�@Co@C@B��@B~�@BM�@B=q@B�@A�@A�#@A��@A%@A%@@��@@Q�@@b@@  @?�;@?�w@?�@?��@?|�@?l�@?K�@>��@>�R@=@=`B@<�@<��@<j@<I�@;ƨ@;@:-@9��@97L@9�@9%@8�`@8Ĝ@8��@8�u@8bN@81'@8 �@7�@7��@7�P@7
=@6v�@6E�@6@5��@5��@5@5@5��@5�h@5O�@5O�@5O�@5`B@5`B@5`B@5O�@5/@5�@5�@4��@4�/@4��@4�@4�D@4I�@49X@41@3��@3�F@3S�@3"�@2��@2��@2~�@2J@1G�@0�`@0�@0b@/+@.{@-�@-/@,��@,z�@+�F@*�H@*��@*�\@*�\@*~�@*^5@*-@*J@)�@)�#@)��@)�#@)��@)��@)��@)x�@)hs@)X@)�@(b@'l�@'+@&�y@&��@%��@$�@$��@$9X@$1@#�m@#�@#C�@#33@#"�@"�@"�\@!��@!��@ ��@ �9@ Q�@�@��@�+@v�@V@5?@$�@{@�@�@�T@��@�@j@��@�m@ƨ@t�@o@�@��@��@��@��@�!@�!@�!@�!@��@�\@n�@-@�^@G�@�@�@��@Ĝ@�u@�@A�@b@�;@��@��@�P@�P@|�@|�@\)@\)@K�@�@�R@ff@5?@@�@��@p�@O�@/@�@I�@��@�m@�m@ƨ@�F@�@33@��@M�@-@�@�^@hs@X@%@�9@�u@�@r�@bN@b@|�@l�@l�@K�@;d@+@��@�R@�+@ff@{@@�-@��@��@��@p�@V@�@�j@�@�@��@z�@z�@Z@9X@�F@C�@
�@
^5@	x�@	G�@	G�@	G�@	7L@	&�@	�@��@�`@�`@�u@1'@ �@b@�;@��@|�@�P@\)@;d@+@+@+@+@+@+@�@
=@��@�y@�R@v�@�T@��@@�-@@@�-@�-@�-@��@�h@`B@V@��@z�@Z@I�@1@�
@�@C�@33@�@��@~�@~�@^5@�@��@��@��@�7@x�@7L@&�@%@ ��@ ��@ �u@ r�@ A�@ A�@ A�@ A�@ A�@ A�@ A�@ A�@  �?�|�?��R?��R?��R?���?�v�?�V?�V?���?�p�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��wA��-A�x�A�VA�33A��A���A��HA���A��9A��hA�/A��A��A�x�A�7LA��9A�ZA��TA�A���A�JA��A�l�A���A�A�/A�~�A�O�A�VA��A���A��A�hsA��A�S�A�
=A���A�ffA�bNA��A��DA�+A�1A�=qA�\)A��
A�A�~�A���A�ZA��HA� �A�jA�p�A��A��uA��A���A���A�~�A�jA�O�A�1'A���A�ƨA�l�A���A�I�A�  A��A���A��A�VA��wA�ĜA�A~�!A}�TA}�7A}/A|��A|AzI�Av��As��Asl�Ar�HAq�TAq�ApbNAo��An��Am��Ak�AkVAjA�AiAg�Ag|�AgVAfVAd�+Ac�#Ac|�Ab�\A`ĜA_�A^z�A^E�A]|�A[K�AZ{AX��AV�!AU��AU�AT�ASVARr�AP�`AO�AM|�AKAI��AH��AG`BAFE�AF  AC�^AB�HABr�AB5?AB  AA�A@�yA@��A@n�A@ �A?�A=��A<VA;�A;��A;&�A8�uA7��A7��A7|�A7l�A7oA6��A6��A5�A4��A3K�A2^5A1A1G�A0�A0z�A01'A0{A/�wA/hsA.JA-�PA,�yA*��A(ffA'33A%�A#�A#�A!�A!/A 1'A�A��A�TAhsAn�A��A1'A�AA�\An�A(�A��A��A�A7LA�A�RA/A5?A�mA�A��AJA��A�hAVA�A�A=qA�hAVA	ƨA��A�A�7A~�A�#At�A�RA9XAƨAG�A�;A �A 9X@�ƨ@�ȴ@��-@�Z@��y@�V@��@��@�r�@�/@�Z@�(�@�1@�ƨ@�dZ@�v�@�1@�{@�V@�j@�9X@�dZ@�E�@�x�@���@�@㝲@��T@�&�@�Ĝ@�bN@���@��@��@�A�@ץ�@�S�@ָR@���@թ�@�p�@�Ĝ@� �@Ӯ@���@Ѻ^@��/@���@��y@�X@̛�@�r�@�  @�K�@�M�@�@ə�@�`B@��/@�9X@ǶF@ǝ�@�l�@�"�@Ə\@�E�@���@��;@+@���@��@�ƨ@���@�l�@�K�@�"�@��@���@�M�@���@�Z@��y@�J@�p�@��u@��@��!@�p�@��j@�1@���@���@�n�@��-@�Z@���@��@��H@��R@�{@���@�p�@���@���@�"�@�v�@���@��@�o@�{@��h@�7L@�1'@��P@�\)@���@��@�r�@���@�@��!@�v�@�=q@�{@���@��@���@�A�@��w@��H@�M�@�-@�@�p�@���@�Ĝ@���@�@���@�V@���@�9X@��
@��P@�S�@�o@��R@�M�@��@�x�@�&�@��`@��D@�Q�@��m@�+@��H@�ȴ@��R@�~�@�V@�M�@���@�Ĝ@��u@���@���@��u@�r�@�1'@��m@��@��@���@�n�@�-@���@��9@�"�@�^5@��@���@���@���@��h@�p�@�`B@��@���@�Q�@� �@���@��
@�ƨ@��@�K�@�
=@��R@��\@�M�@�-@��@�@�hs@��@��`@���@��D@��@�A�@�w@K�@~��@~�R@~V@~$�@}�@}��@}�-@}��@}��@}�@}`B@}?}@|�@|�j@|j@|(�@{��@z�\@yx�@x��@xĜ@x�9@x�u@xbN@xQ�@xA�@xA�@xA�@x �@xb@w�@w��@w�w@w��@w�P@wl�@w\)@w�@v�@vV@u�T@u/@t��@tI�@sdZ@r��@r-@q�@q��@q�7@qX@q7L@q�@p��@p�`@p�9@pbN@o�;@o��@o\)@ol�@o|�@oK�@m��@l�@lj@k��@kƨ@kS�@kC�@ko@j�!@j~�@jM�@i�@ix�@i%@h�`@hĜ@h�9@h��@h��@h�u@h�@h�@hr�@hbN@hQ�@hQ�@hA�@hA�@h1'@hb@g�@g�w@g��@gl�@f�R@f@e`B@d�j@dI�@d�@c��@c@b��@b~�@bM�@a��@a��@a7L@` �@_�@_��@_�@_l�@_;d@_
=@^�R@^V@]�T@]��@]p�@\z�@\1@[��@[�m@[�m@[�m@[ƨ@[��@[dZ@[C�@[33@[@Z��@Zn�@Z-@Y��@Y�@W�;@Vȴ@Vȴ@Vȴ@V�R@V��@V�+@V@U�-@T�j@S�
@S�F@St�@R�@R�\@R^5@R-@RJ@Q�@Q�#@Q&�@P�`@PĜ@PĜ@P��@P�u@P�@P�@P�@P�@Pr�@P�@P�@P�@P�@P �@Nȴ@M�@MO�@M?}@M�@L�@LZ@LI�@LZ@LI�@L(�@K��@K�F@K��@KS�@K@J�!@JJ@IX@H�9@Hr�@Hb@G�;@G�@G�P@G|�@F��@E��@EO�@D�j@C��@C�m@C��@Ct�@CS�@CS�@CC�@CC�@CC�@C"�@Co@C@B��@B~�@BM�@B=q@B�@A�@A�#@A��@A%@A%@@��@@Q�@@b@@  @?�;@?�w@?�@?��@?|�@?l�@?K�@>��@>�R@=@=`B@<�@<��@<j@<I�@;ƨ@;@:-@9��@97L@9�@9%@8�`@8Ĝ@8��@8�u@8bN@81'@8 �@7�@7��@7�P@7
=@6v�@6E�@6@5��@5��@5@5@5��@5�h@5O�@5O�@5O�@5`B@5`B@5`B@5O�@5/@5�@5�@4��@4�/@4��@4�@4�D@4I�@49X@41@3��@3�F@3S�@3"�@2��@2��@2~�@2J@1G�@0�`@0�@0b@/+@.{@-�@-/@,��@,z�@+�F@*�H@*��@*�\@*�\@*~�@*^5@*-@*J@)�@)�#@)��@)�#@)��@)��@)��@)x�@)hs@)X@)�@(b@'l�@'+@&�y@&��@%��@$�@$��@$9X@$1@#�m@#�@#C�@#33@#"�@"�@"�\@!��@!��@ ��@ �9@ Q�@�@��@�+@v�@V@5?@$�@{@�@�@�T@��@�@j@��@�m@ƨ@t�@o@�@��@��@��@��@�!@�!@�!@�!@��@�\@n�@-@�^@G�@�@�@��@Ĝ@�u@�@A�@b@�;@��@��@�P@�P@|�@|�@\)@\)@K�@�@�R@ff@5?@@�@��@p�@O�@/@�@I�@��@�m@�m@ƨ@�F@�@33@��@M�@-@�@�^@hs@X@%@�9@�u@�@r�@bN@b@|�@l�@l�@K�@;d@+@��@�R@�+@ff@{@@�-@��@��@��@p�@V@�@�j@�@�@��@z�@z�@Z@9X@�F@C�@
�@
^5@	x�@	G�@	G�@	G�@	7L@	&�@	�@��@�`@�`@�u@1'@ �@b@�;@��@|�@�P@\)@;d@+@+@+@+@+@+@�@
=@��@�y@�R@v�@�T@��@@�-@@@�-@�-@�-@��@�h@`B@V@��@z�@Z@I�@1@�
@�@C�@33@�@��@~�@~�@^5@�@��@��@��@�7@x�@7L@&�@%@ ��@ ��@ �u@ r�@ A�@ A�@ A�@ A�@ A�@ A�@ A�@ A�@  �?�|�?��R?��R?��R?���?�v�?�V?�V?���?�p�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�
B��B�
B�
B�B�#B�#B�/B�#B�B�B�#B�BB�BB�/B��B�?B��B�=B�=B�%B�1B�Bl�BffBiyBP�BN�B[#BE�B:^BB�B6FB,B@�B9XB/B�BbB�BPB�B�/B�BB��BƨB�B��Bv�B�{B�7Bw�BcTBQ�B@�B-B�BPB'�B)�B&�B"�B�B\B
��B
�
B
�RB
ɺB
��B
ÖB
�B
��B
�hB
� B
^5B
ZB
ffB
ffB
gmB
dZB
XB
I�B
0!B
	7B	�B
�B
�B
DB
B
B	��B	�B	�ZB	�B	�B	��B	ǮB	ȴB	ɺB	ǮB	�jB	�B	�!B	�-B	��B	�oB	�DB	��B	��B	�%B	hsB	ffB	gmB	XB	`BB	ffB	XB	L�B	I�B	33B	$�B	�B	bB	%B	
=B	B��B	B�B�B	B	B	  B��B��B��B��B�B�TB��B��B�/B�BB��B�'BȴB�5B�)B�B��B��B��B�}B�'B��B�B�'B�3B�B�9B�!B�B��B��B�DB�hB�+BgmBXBgmBdZB^5BjB`BBhsB_;BiyBaHBZBaHBQ�BK�BYB_;BT�B[#B`BB\)BXBN�B<jBE�BM�BK�B8RB7LBG�BC�B<jB=qBD�BA�B7LB,B)�B,B1'B(�B �B�B$�B'�B�B$�B(�B!�B$�B!�B�BbB�B �B%�B"�B!�B�B�B�B�B�BPB	7B%�B0!B/B-B'�B�B{B�B&�B.B)�B&�B#�B&�B)�B'�B"�B�B%�B'�B"�B�BbBDB�B%�B(�B%�B#�B,B)�B$�B#�B$�B �B�B�B�B �B�B&�B/B+B'�B&�B,B1'B0!B-B-B/B49B2-B0!B,B,B'�B�B�B#�B0!B=qB>wB=qB>wB>wB=qB:^B33B1'B.B/B9XB;dB;dB;dB7LB7LBB�BA�BD�B@�BI�BB�B@�BH�BM�BR�BQ�BN�BO�BR�BN�BK�BT�BS�BT�BS�BP�B^5BgmBhsBe`BjBo�Bl�BhsBm�Bq�Bw�B{�B~�B}�B~�B{�B}�B� B~�B� B�B�+B�PB�PB�7B�DB�PB�%B�B��B��B��B��B��B��B�B�B�B�B�B�'B�?B�LB�LB�^B�XB�jBÖBȴBɺBȴBȴBɺBƨBŢB�B�B�B�
B�B��B�
B�B�B�;B�ZB�NB�BB�/B�TB��B	B	B	B	+B	1B	1B		7B	1B	1B	\B	oB	{B	�B	�B	�B	�B	�B	�B	 �B	"�B	%�B	'�B	&�B	,B	0!B	5?B	7LB	<jB	=qB	;dB	>wB	C�B	G�B	H�B	I�B	L�B	M�B	P�B	R�B	VB	W
B	W
B	W
B	XB	XB	YB	YB	ZB	ZB	[#B	`BB	gmB	k�B	l�B	l�B	m�B	n�B	o�B	o�B	o�B	o�B	o�B	o�B	p�B	p�B	q�B	q�B	q�B	q�B	p�B	r�B	s�B	u�B	w�B	y�B	}�B	|�B	�B	�%B	�DB	�JB	�JB	�PB	�PB	�VB	�VB	�VB	�PB	�VB	�VB	�oB	�uB	��B	��B	��B	�hB	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�!B	�'B	�3B	�LB	�RB	�XB	�XB	�^B	�^B	�^B	�dB	�dB	�dB	�jB	�jB	�jB	�jB	�jB	�jB	�jB	�jB	�qB	�jB	�dB	�wB	��B	ÖB	ȴB	��B	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�/B	�#B	�#B	�BB	�ZB	�ZB	�ZB	�ZB	�TB	�TB	�ZB	�`B	�`B	�ZB	�ZB	�ZB	�ZB	�ZB	�TB	�TB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
%B
%B
%B
B
B
B
B
B
B
B
B
B
+B

=B

=B
JB
JB
JB
DB
1B
	7B
VB
VB
\B
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
uB
�B
�B
�B
�B
�B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
"�B
$�B
%�B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
$�B
#�B
$�B
'�B
(�B
(�B
)�B
)�B
+B
)�B
)�B
)�B
,B
,B
,B
,B
+B
+B
+B
+B
+B
+B
+B
,B
+B
+B
+B
,B
,B
,B
+B
+B
,B
,B
-B
-B
+B
)�B
-B
-B
-B
,B
,B
1'B
33B
49B
33B
2-B
49B
8RB
9XB
:^B
:^B
9XB
9XB
:^B
:^B
;dB
;dB
;dB
;dB
;dB
;dB
;dB
;dB
:^B
8RB
6FB
9XB
<jB
<jB
<jB
;dB
;dB
@�B
@�B
A�B
B�B
B�B
B�B
C�B
C�B
B�B
A�B
A�B
A�B
B�B
D�B
D�B
A�B
F�B
J�B
J�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
I�B
G�B
E�B
J�B
M�B
M�B
L�B
L�B
N�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
O�B
O�B
N�B
N�B
M�B
M�B
N�B
P�B
Q�B
P�B
P�B
Q�B
R�B
Q�B
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
S�B
S�B
R�B
S�B
T�B
VB
VB
VB
T�B
VB
VB
T�B
T�B
W
B
YB
YB
YB
YB
XB
W
B
W
B
YB
ZB
[#B
YB
ZB
[#B
[#B
[#B
\)B
]/B
]/B
]/B
\)B
[#B
^5B
_;B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
^5B
_;B
aHB
aHB
aHB
aHB
`BB
_;B
bNB
aHB
bNB
bNB
bNB
bNB
bNB
aHB
aHB
_;B
_;B
`BB
`BB
`BB
e`B
gmB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
e`B
e`B
gmB
gmB
gmB
gmB
iyB
jB
jB
k�B
l�B
m�B
m�B
m�B
m�B
m�B
l�B
l�B
l�B
l�B
l�B
k�B
k�B
n�B
o�B
o�B
p�B
o�B
o�B
o�B
o�B
o�B
n�B
n�B
m�B
m�B
o�B
o�B
o�B
o�B
n�B
n�B
n�B
o�B
n�B
o�B
o�B
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
s�B
s�B
s�B
r�B
s�B
s�B
t�B
u�B
u�B
t�B
t�B
t�B
s�B
s�B
q�B
s�B
v�B
v�B
v�B
v�B
v�B
v�B
u�B
u�B
v�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�?BԕB�sB�sB�kB�qB�WB�~BۦBڠB��B��B��B��B�5B��B��B��B�BB��B�	B�xB�EBp�Bi�Bk�BT�BQhB\)BH1B<�BC�B8�B-�BAB:^B0�B_B�B�B�B��B߾B�4B�B�fB��B��Bz�B�B��By�Be`BTFBB�B/�B�BB(>B*eB'8B#:B)B.B OB
�kB
�B
��B
ϫB
��B
�CB
��B
��B
�AB
bhB
\CB
g�B
gmB
h
B
d�B
YB
KB
2�B
6B	�%B
B
SB
�B
3B
�B	�	B	��B	�B	�#B	�?B	�B	�B	��B	�rB	�fB	��B	�B	�B	��B	�bB	��B	�PB	�YB	�1B	��B	k6B	h>B	i_B	Z�B	a�B	gRB	YB	NVB	J�B	5tB	'RB	�B	�B	�B	�B	�B	 �B	B�/B��B	�B	�B	 �B��B��B�XB�RB�iB�B�VB�}B��B��B�TB�nBɺB�jB�xB�kBԕB�hB�xB��B�B��B�5B�B��B��B��B��B��B��B��B�B�TB��BjB[	BiDBf�B`vBk�Ba�Bi�B`�BjeBb�B[�BbBS�BM�BZB_�BVmB[�B`�B\�BX�BO�B>wBF�BNpBL~B:xB8�BH1BDgB=�B>]BEBB'B8RB-�B+�B-CB2-B*0B"�BB%�B(�B 'B%�B)�B"�B%�B"�B �BoB�B!�B&�B#�B"�B�B�B�B~B�B(BB&fB0UB/iB-wB(�B �BSB�B'�B.}B*�B'�B$�B'�B*B(�B#�B�B&�B(sB#�B�B�B6B�B&fB)�B&�B$�B,WB*eB%�B$tB%`B!�B�B�B�B!�B�B'�B/iB+�B(�B'�B,�B1�B0�B-�B-�B/�B4nB2�B0�B,�B,�B(�B1B �B%B0�B=�B>�B=�B>�B>�B=�B:�B3�B1�B/OB0;B:B<B<6B<6B8lB8lBCBBABE9BAUBJ	BCaBA�BIRBNpBS@BRTBOvBP}BS[BO�BL�BU�BT�BU�BT�BR:B_Bh
Bh�Bf2BkBpBmCBiyBn�BraBxRB|PBHB~]BcB|�B~wB��B�B��B��B��B��B��B��B��B��B�EB�tB��B�_B�BB�@B�LB�_B�QB��B�qB��B��B��B��B��B��B��B��B�B��B��B�	B�B�B�	B�EBƎB�B�7B�KB�?B�SB�gB�sBؓBڠBߤB�B�B��B�OB�B�tB	UB	MB	mB	zB	fB	�B		lB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	B	!B	# B	&B	($B	'RB	,qB	0�B	5�B	7�B	<�B	=�B	;�B	>�B	C�B	G�B	H�B	J	B	MB	NB	QB	S@B	VB	W?B	W?B	W?B	XEB	X_B	YKB	YB	Z�B	Z�B	[�B	`�B	g�B	k�B	l�B	l�B	m�B	n�B	o�B	o�B	o�B	o�B	o�B	o�B	p�B	p�B	q�B	q�B	q�B	q�B	p�B	r�B	tB	vB	x8B	zDB	~BB	}�B	�{B	�tB	�xB	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�,B	�2B	�0B	�kB	�/B	�cB	�]B	�OB	�UB	��B	��B	��B	��B	��B	�rB	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�B	�B	�.B	�B	�4B	� B	�bB	�bB	�EB	�eB	�KB	�KB	�QB	�QB	�eB	�eB	�qB	�~B	یB	ۦB	�vB	�tB	�tB	�B	�tB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	��B	�
B	�B	�B	��B	��B	��B	��B	��B	�B	� B	��B	��B	�B	�B	�B	�	B	�	B	�B	�B	�2B	�B	�"B	�B	�<B	�B	�(B	�B	�(B	�B	�(B	�B	�B	�"B	�6B	�>B	�`B	�PB	�HB
3B
GB
[B
MB
?B
?B
?B
SB
SB
MB
SB
mB
SB
gB
�B
�B
zB

rB

�B
�B
�B
~B
xB
�B
	�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B

B
	B
B
�B
�B
�B
�B
	B
B
 B
#B
%B
%�B
%,B
%B
&B
&B
&B
&B
&B
&B
&B
%B
$&B
%,B
($B
)*B
)*B
*B
*B
+6B
*KB
*KB
*0B
,"B
,"B
,=B
,"B
+B
+B
+6B
+6B
+B
+6B
+6B
,"B
+QB
+6B
+6B
,"B
,=B
,WB
+6B
+QB
,=B
,=B
-CB
-CB
+QB
*eB
-]B
-]B
-]B
,�B
,�B
1�B
3�B
4nB
3�B
2�B
4�B
8�B
9rB
:xB
:�B
9�B
9�B
:�B
:�B
;B
;B
;B
;B
;�B
;�B
;�B
;B
:�B
8�B
6�B
9�B
<�B
<�B
<�B
;�B
;�B
@�B
@�B
A�B
B�B
B�B
B�B
C�B
C�B
B�B
A�B
A�B
A�B
B�B
D�B
D�B
B'B
F�B
J�B
J�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
I�B
HB
F%B
J�B
M�B
N"B
MB
MB
OB
O�B
O�B
O�B
Q B
Q B
Q B
Q B
O�B
O�B
OB
OB
NB
N"B
OBB
QB
RB
QB
QB
R B
S@B
R B
S&B
S&B
TB
T,B
UB
UB
UB
U2B
U2B
U2B
TFB
T,B
S@B
T,B
U2B
V9B
V9B
VSB
UMB
V9B
V9B
UMB
UMB
W?B
Y1B
Y1B
YKB
YKB
XEB
WYB
WYB
YeB
ZQB
[=B
YKB
ZQB
[WB
[qB
[WB
\]B
]IB
]IB
]dB
\]B
[�B
^OB
_pB
^�B
^OB
^jB
^jB
^jB
^OB
_pB
^jB
_pB
abB
abB
abB
abB
`vB
_pB
bhB
a|B
bhB
bhB
b�B
b�B
bhB
a|B
a|B
_�B
_�B
`�B
`�B
`�B
ezB
g�B
f�B
f�B
f�B
f�B
f�B
f�B
f�B
e�B
e�B
g�B
g�B
g�B
g�B
i�B
j�B
j�B
k�B
l�B
m�B
m�B
m�B
m�B
m�B
l�B
l�B
l�B
l�B
l�B
k�B
k�B
n�B
o�B
o�B
p�B
o�B
o�B
o�B
o�B
o�B
n�B
n�B
m�B
m�B
o�B
o�B
o�B
o�B
n�B
n�B
n�B
o�B
n�B
o�B
o�B
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
tB
s�B
tB
r�B
tB
s�B
t�B
u�B
u�B
t�B
t�B
t�B
s�B
tB
q�B
s�B
v�B
v�B
v�B
v�B
v�B
v�B
u�B
u�B
v�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.22(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201806140033302018061400333020180614003330201806221243132018062212431320180622124313201806150023032018061500230320180615002303  JA  ARFMdecpA19c                                                                20180610093502  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180610003504  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180610003506  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180610003507  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180610003508  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180610003508  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180610003508  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180610003508  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180610003508  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180610003508                      G�O�G�O�G�O�                JA  ARUP                                                                        20180610005538                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180610153524  CV  JULD            G�O�G�O�F�I�                JM  ARCAJMQC2.0                                                                 20180613153330  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180613153330  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180614152303  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622034313  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115131518                      G�O�G�O�G�O�                