CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2022-01-24T08:01:11Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p@   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �|   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �h   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �L   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �8   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �<   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �@   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �D   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �H   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �L   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220124080111  20220124080111  4903032 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  7212                            2B  A   NAVIS_A                         0942                            170425                          863 @ٴS�X�1   @ٴT$��@<��n���c�z�G�1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         �A   A   F   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�<�DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�<�Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D޼�D�  D�@ D߀ D�� D�  D�<�D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D���D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @xQ�@���@���Az�A<z�A\z�A|z�A�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qB�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\BÏ\BǏ\Bˏ\BϏ\Bӏ\B׏\Bۏ\Bߏ\B�\B�\B�\B�\B�\B��\B��\B��\CǮCǮCǮCǮC	ǮCǮCǮCǮCǮCǮCǮCǮCǮCǮCǮCǮC!ǮC#ǮC%ǮC'ǮC)ǮC+ǮC-ǮC/ǮC1ǮC3ǮC5ǮC7ǮC9ǮC;ǮC=ǮC?ǮCAǮCCǮCEǮCGǮCIǮCKǮCMǮCOǮCQǮCSǮCUǮCWǮCYǮC[ǮC]ǮC_ǮCaǮCcǮCeǮCgǮCiǮCkǮCmǮCoǮCqǮCsǮCuǮCwǮCyǮC{ǮC}ǮCǮC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C��C���C���C���C���D q�D ��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��D	q�D	��D
q�D
��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��D q�D ��D!q�D!��D"q�D"��D#q�D#��D$q�D$��D%q�D%��D&q�D&��D'q�D'��D(q�D(��D)q�D)��D*q�D*��D+q�D+��D,q�D,��D-q�D-��D.q�D.��D/q�D/��D0q�D0��D1q�D1��D2q�D2��D3q�D3��D4q�D4��D5q�D5��D6q�D6��D7q�D7��D8q�D8��D9q�D9��D:q�D:��D;q�D;��D<q�D<��D=q�D=��D>q�D>��D?q�D?��D@q�D@��DAq�DA��DBq�DB��DCq�DC��DDq�DD��DEq�DE��DFq�DF��DGq�DG��DHq�DH��DIq�DI��DJq�DJ��DKq�DK��DLq�DL��DMq�DM��DNq�DN��DOq�DO��DPq�DP��DQq�DQ��DRq�DR��DSq�DS��DTq�DT��DUq�DU��DVq�DV��DWq�DW��DXq�DX��DYq�DY��DZq�DZ��D[q�D[��D\q�D\��D]q�D]��D^q�D^��D_q�D_��D`q�D`��Daq�Da��Dbq�Db��Dcq�Dc��Ddq�Dd��Deq�De��Dfq�Df��Dgq�Dg��Dhq�Dh��Diq�Di��Djq�Dj��Dkq�Dk��Dlq�Dl��Dmq�Dm��Dnq�Dn��Doq�Do��Dpq�Dp��Dqq�Dq��Drq�Dr��Dsq�Ds��Dtq�Dt��Duq�Du��Dvq�Dv��Dwq�Dw��Dxq�Dx��Dyq�Dy��Dzq�Dz��D{q�D{��D|q�D|��D}q�D}��D~q�D~��Dq�D��D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D¸�D���D�8�D�x�Dø�D���D�5�D�x�Dĸ�D���D�8�D�x�DŸ�D���D�8�D�x�DƸ�D���D�8�D�x�DǸ�D���D�8�D�x�Dȸ�D���D�8�D�x�Dɸ�D���D�8�D�x�Dʸ�D���D�8�D�x�D˸�D���D�8�D�x�D̸�D���D�8�D�x�D͸�D���D�8�D�x�Dθ�D���D�8�D�x�Dϸ�D���D�8�D�x�Dи�D���D�8�D�x�DѸ�D���D�8�D�x�DҸ�D���D�8�D�x�DӸ�D���D�8�D�x�DԸ�D���D�8�D�x�Dո�D���D�8�D�x�Dָ�D���D�8�D�x�D׸�D���D�8�D�x�Dظ�D���D�8�D�x�Dٸ�D���D�8�D�x�Dڸ�D���D�5�D�x�D۸�D���D�8�D�x�Dܸ�D���D�8�D�x�Dݸ�D���D�8�D�x�D޵�D���D�8�D�x�D߸�D���D�5�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D���D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��)D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D���D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�oA�{A�{A��A��A��A�oA���A���A��A��`A��TA��HA��HA��TA��TA��TA��TA��`A��`A��`A��`A��`A��`A��`A��mA��mA��mA��mA��`A��`A��`A��`A��`A��`A��`A��#A���A�ȴA�ƨA�ƨA�ȴA�ȴA���A�ƨA���A��A�t�A��^A��A��uA��+A��mA�S�A�1A�{A���A���A�x�A�VA�M�A�r�A��yA�^5A��A���A�bA��RA���A�A�A���A�+A���A�E�A���A�I�A��^A�oA���A��!A���A��TA���A�dZA���A�A���A� �A�hsA��hA�oA�A|-AzM�Aw�mAv�AvQ�AvbAt��Ar~�Aq�FAq�Ao�mAn��Al��Ak�7Ai�wAh��Ag/AeC�Ad��Ac|�Aa�TA`r�A_�
A^��A]�A\�+A[�A[��A[/AZ�AY��AX�jAW&�AUl�AT��AT~�ATr�AS|�AR  APr�AO
=ANr�AM��AK��AKhsAK"�AJ�yAJ��AJ��AIdZAH�yAH9XAG�mAG�^AGl�AF�!AD�DAC;dAB�AA�wAA�hAAXA@��A?7LA>r�A<=qA;��A;+A:�A9�
A9
=A8��A8 �A6M�A5p�A5�A4�`A4�9A4n�A4A3��A3G�A2�A2�RA2ffA1XA0bA/�wA/\)A.~�A-��A+��A++A)x�A(�A(jA(  A'�wA'��A't�A&VA$�A#�hA"�A"�+A"-A!�TA!�PA!�A �yA ��A JA��A��A;dA�A�A�A&�A~�A�At�AAJA^5AK�AĜAVA�A�9AAv�A�A�A�^A�hAdZA
=A �AhsA
�/A
9XA	�FA	;dA�HA��AȴA�9AJA~�Av�A1A��A��AffA��A%A $�@�+@��H@��@�33@�X@�  @��H@�%@�-@���@�J@��@�@�9@�\@�@�I�@㝲@�
=@�n�@��@�p�@��@��;@�C�@��@�p�@��;@ۍP@ڸR@��@� �@��@�v�@�5?@���@�;d@с@Л�@��H@��@�  @�K�@�v�@ɑh@� �@�C�@�v�@ź^@�Z@�M�@���@�z�@��
@�C�@�ȴ@�O�@���@���@�o@�%@���@�"�@���@�x�@��@��F@�o@�-@�X@�Ĝ@��m@�l�@�@���@��@���@�  @��H@��^@�G�@�V@�Z@�o@�V@���@��@�b@���@�dZ@��@��y@�ff@��7@�p�@��`@�1'@��@�@���@��@���@��+@�V@�5?@�@���@��@�X@��@�Ĝ@���@�33@���@�V@�@�G�@�&�@��@��@��j@�Q�@���@���@�@�`B@��/@�r�@�Q�@�1'@� �@��@��P@��@��!@��@���@���@�p�@�/@��j@�b@��F@���@�+@��R@���@��\@�M�@�@��@���@���@��7@�x�@�hs@�7L@��@��@��`@���@�j@�1'@���@�ƨ@��P@��@�"�@��\@�=q@��@��^@��h@�hs@��@���@��@�Z@� �@�1@�1@���@���@�\)@�K�@�33@��@��H@�^5@�$�@��T@��@�X@�/@��@���@�Ĝ@�r�@��@��m@���@�\)@���@���@�@���@��^@�x�@�/@���@���@��D@�r�@�A�@��@K�@�@~�@~ff@}�T@}�@|��@|z�@|1@{ƨ@{��@{33@z�@z��@z�\@z-@y��@y�#@y��@y�^@y�@x��@xr�@x1'@w��@w|�@w;d@w;d@v�R@vV@v@u/@t�D@s��@s33@s"�@s"�@rn�@q�#@q��@qhs@q&�@p�`@p�u@pQ�@pb@o�;@ol�@n�y@nff@n$�@n@m�-@mO�@l�/@l�D@lZ@l1@kt�@j��@j=q@i�7@i�7@i��@iX@hĜ@h�@hQ�@h �@g�@g�;@g��@g�w@g;d@f��@f��@fv�@fff@f5?@e�@e�-@e��@ep�@eV@d�@d�D@dj@dZ@c�
@c��@cC�@b��@b~�@bJ@a�#@a�7@a�@`��@`��@`��@`Ĝ@`�9@`A�@`  @_��@_l�@_+@^��@^ȴ@^��@^v�@^V@]@]�@]?}@]/@]V@\�/@\9X@[�m@[�m@[�m@[�m@[�
@[t�@[@Z�!@Z~�@Z-@Y��@Yx�@Y7L@X�`@Xr�@W�;@W�P@W�@V�y@Vȴ@VV@U��@U?}@T�@T�@T�@Tj@TI�@Sƨ@S�@S"�@R�@R��@R��@R^5@Q�#@Qhs@Q&�@Q%@P��@P��@PbN@PA�@Pb@O��@Ol�@O�@Nȴ@N�+@NV@N5?@N@M��@M@M�h@MO�@M�@L�/@L�@LI�@L�@Kƨ@K��@KS�@K@J��@J-@J�@I��@I�#@I��@I�7@H��@H��@HbN@H1'@G�w@G|�@G+@Fȴ@F�+@FV@F$�@E�T@E@Ep�@E?}@D��@D�j@D�D@Dz�@DZ@D9X@D1@C�m@C�F@CC�@B��@B^5@BM�@A��@A7L@A%@@�`@@��@@bN@?��@?|�@?;d@>��@>ȴ@>�+@>ff@>5?@>{@=��@=��@=`B@=?}@=V@<��@<�@<�D@<9X@;��@;ƨ@;S�@;@:n�@:=q@:J@9�#@9�^@9x�@97L@9�@8�`@8�9@8�9@8�u@8�@8Q�@8A�@8  @7�;@7��@7�@7�P@7+@6��@6�R@6ff@6V@5�@5@5��@5�h@5/@4��@4�j@4��@4I�@3�m@3��@3t�@3o@2��@2��@2��@2��@2�\@2=q@2J@1��@1�#@1��@1�^@1��@1x�@1hs@1X@0��@0�9@0�@01'@/�@/�w@/�@/�P@/�P@/l�@/K�@/
=@.�@.�+@.E�@-�@-�@-O�@-�@,�/@,��@,�j@,j@,9X@+��@+t�@+@*n�@*=q@*�@*�@*J@*J@)�@)x�@)&�@)%@(�`@(bN@( �@(b@'�@'�w@'|�@'K�@';d@'
=@&�@&�@&ȴ@&��@&V@&$�@&@%��@%p�@%�@%V@$z�@$(�@#�m@#ƨ@#dZ@"�@"��@"��@"-@!�@!��@!�7@!G�@!%@ Ĝ@ r�@ A�@�;@�@�P@;d@�@�@
=@��@�@ff@5?@�T@�-@�h@`B@/@�/@��@��@Z@9X@��@��@"�@��@~�@M�@J@�#@��@x�@hs@X@G�@7L@&�@7L@�@��@Ĝ@Ĝ@Ĝ@�9@�u@A�@1'@ �@b@  @�;@�w@�@��@\)@;d@��@�R@��@V@5?@�@@�-@`B@?}@?}@/@�@V@�/@��@�@j@(�@�@1@��@�@dZ@"�@�@�H@�!@��@~�@n�@n�@^5@=q@-@J@��@��@hs@G�@&�@�@%@%@%@�`@Ĝ@��@r�@A�@ �@�@�w@�@��@\)@
=@�@�R@��@�+@ff@E�@@��@@@��@p�@�@`B@/@��@��@�@�@�/@�j@��@Z@�@�@��@�
@�F@��@dZ@33@"�@o@
�@
��@
�\@
M�@
=q@
-@
J@	�#@	��@	��@	hs@��@�9@��@�u@r�@bN111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�oA�{A�{A��A��A��A�oA���A���A��A��`A��TA��HA��HA��TA��TA��TA��TA��`A��`A��`A��`A��`A��`A��`A��mA��mA��mA��mA��`A��`A��`A��`A��`A��`A��`A��#A���A�ȴA�ƨA�ƨA�ȴA�ȴA���A�ƨA���A��A�t�A��^A��A��uA��+A��mA�S�A�1A�{A���A���A�x�A�VA�M�A�r�A��yA�^5A��A���A�bA��RA���A�A�A���A�+A���A�E�A���A�I�A��^A�oA���A��!A���A��TA���A�dZA���A�A���A� �A�hsA��hA�oA�A|-AzM�Aw�mAv�AvQ�AvbAt��Ar~�Aq�FAq�Ao�mAn��Al��Ak�7Ai�wAh��Ag/AeC�Ad��Ac|�Aa�TA`r�A_�
A^��A]�A\�+A[�A[��A[/AZ�AY��AX�jAW&�AUl�AT��AT~�ATr�AS|�AR  APr�AO
=ANr�AM��AK��AKhsAK"�AJ�yAJ��AJ��AIdZAH�yAH9XAG�mAG�^AGl�AF�!AD�DAC;dAB�AA�wAA�hAAXA@��A?7LA>r�A<=qA;��A;+A:�A9�
A9
=A8��A8 �A6M�A5p�A5�A4�`A4�9A4n�A4A3��A3G�A2�A2�RA2ffA1XA0bA/�wA/\)A.~�A-��A+��A++A)x�A(�A(jA(  A'�wA'��A't�A&VA$�A#�hA"�A"�+A"-A!�TA!�PA!�A �yA ��A JA��A��A;dA�A�A�A&�A~�A�At�AAJA^5AK�AĜAVA�A�9AAv�A�A�A�^A�hAdZA
=A �AhsA
�/A
9XA	�FA	;dA�HA��AȴA�9AJA~�Av�A1A��A��AffA��A%A $�@�+@��H@��@�33@�X@�  @��H@�%@�-@���@�J@��@�@�9@�\@�@�I�@㝲@�
=@�n�@��@�p�@��@��;@�C�@��@�p�@��;@ۍP@ڸR@��@� �@��@�v�@�5?@���@�;d@с@Л�@��H@��@�  @�K�@�v�@ɑh@� �@�C�@�v�@ź^@�Z@�M�@���@�z�@��
@�C�@�ȴ@�O�@���@���@�o@�%@���@�"�@���@�x�@��@��F@�o@�-@�X@�Ĝ@��m@�l�@�@���@��@���@�  @��H@��^@�G�@�V@�Z@�o@�V@���@��@�b@���@�dZ@��@��y@�ff@��7@�p�@��`@�1'@��@�@���@��@���@��+@�V@�5?@�@���@��@�X@��@�Ĝ@���@�33@���@�V@�@�G�@�&�@��@��@��j@�Q�@���@���@�@�`B@��/@�r�@�Q�@�1'@� �@��@��P@��@��!@��@���@���@�p�@�/@��j@�b@��F@���@�+@��R@���@��\@�M�@�@��@���@���@��7@�x�@�hs@�7L@��@��@��`@���@�j@�1'@���@�ƨ@��P@��@�"�@��\@�=q@��@��^@��h@�hs@��@���@��@�Z@� �@�1@�1@���@���@�\)@�K�@�33@��@��H@�^5@�$�@��T@��@�X@�/@��@���@�Ĝ@�r�@��@��m@���@�\)@���@���@�@���@��^@�x�@�/@���@���@��D@�r�@�A�@��@K�@�@~�@~ff@}�T@}�@|��@|z�@|1@{ƨ@{��@{33@z�@z��@z�\@z-@y��@y�#@y��@y�^@y�@x��@xr�@x1'@w��@w|�@w;d@w;d@v�R@vV@v@u/@t�D@s��@s33@s"�@s"�@rn�@q�#@q��@qhs@q&�@p�`@p�u@pQ�@pb@o�;@ol�@n�y@nff@n$�@n@m�-@mO�@l�/@l�D@lZ@l1@kt�@j��@j=q@i�7@i�7@i��@iX@hĜ@h�@hQ�@h �@g�@g�;@g��@g�w@g;d@f��@f��@fv�@fff@f5?@e�@e�-@e��@ep�@eV@d�@d�D@dj@dZ@c�
@c��@cC�@b��@b~�@bJ@a�#@a�7@a�@`��@`��@`��@`Ĝ@`�9@`A�@`  @_��@_l�@_+@^��@^ȴ@^��@^v�@^V@]@]�@]?}@]/@]V@\�/@\9X@[�m@[�m@[�m@[�m@[�
@[t�@[@Z�!@Z~�@Z-@Y��@Yx�@Y7L@X�`@Xr�@W�;@W�P@W�@V�y@Vȴ@VV@U��@U?}@T�@T�@T�@Tj@TI�@Sƨ@S�@S"�@R�@R��@R��@R^5@Q�#@Qhs@Q&�@Q%@P��@P��@PbN@PA�@Pb@O��@Ol�@O�@Nȴ@N�+@NV@N5?@N@M��@M@M�h@MO�@M�@L�/@L�@LI�@L�@Kƨ@K��@KS�@K@J��@J-@J�@I��@I�#@I��@I�7@H��@H��@HbN@H1'@G�w@G|�@G+@Fȴ@F�+@FV@F$�@E�T@E@Ep�@E?}@D��@D�j@D�D@Dz�@DZ@D9X@D1@C�m@C�F@CC�@B��@B^5@BM�@A��@A7L@A%@@�`@@��@@bN@?��@?|�@?;d@>��@>ȴ@>�+@>ff@>5?@>{@=��@=��@=`B@=?}@=V@<��@<�@<�D@<9X@;��@;ƨ@;S�@;@:n�@:=q@:J@9�#@9�^@9x�@97L@9�@8�`@8�9@8�9@8�u@8�@8Q�@8A�@8  @7�;@7��@7�@7�P@7+@6��@6�R@6ff@6V@5�@5@5��@5�h@5/@4��@4�j@4��@4I�@3�m@3��@3t�@3o@2��@2��@2��@2��@2�\@2=q@2J@1��@1�#@1��@1�^@1��@1x�@1hs@1X@0��@0�9@0�@01'@/�@/�w@/�@/�P@/�P@/l�@/K�@/
=@.�@.�+@.E�@-�@-�@-O�@-�@,�/@,��@,�j@,j@,9X@+��@+t�@+@*n�@*=q@*�@*�@*J@*J@)�@)x�@)&�@)%@(�`@(bN@( �@(b@'�@'�w@'|�@'K�@';d@'
=@&�@&�@&ȴ@&��@&V@&$�@&@%��@%p�@%�@%V@$z�@$(�@#�m@#ƨ@#dZ@"�@"��@"��@"-@!�@!��@!�7@!G�@!%@ Ĝ@ r�@ A�@�;@�@�P@;d@�@�@
=@��@�@ff@5?@�T@�-@�h@`B@/@�/@��@��@Z@9X@��@��@"�@��@~�@M�@J@�#@��@x�@hs@X@G�@7L@&�@7L@�@��@Ĝ@Ĝ@Ĝ@�9@�u@A�@1'@ �@b@  @�;@�w@�@��@\)@;d@��@�R@��@V@5?@�@@�-@`B@?}@?}@/@�@V@�/@��@�@j@(�@�@1@��@�@dZ@"�@�@�H@�!@��@~�@n�@n�@^5@=q@-@J@��@��@hs@G�@&�@�@%@%@%@�`@Ĝ@��@r�@A�@ �@�@�w@�@��@\)@
=@�@�R@��@�+@ff@E�@@��@@@��@p�@�@`B@/@��@��@�@�@�/@�j@��@Z@�@�@��@�
@�F@��@dZ@33@"�@o@
�@
��@
�\@
M�@
=q@
-@
J@	�#@	��@	��@	hs@��@�9@��@�u@r�@bN111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�RB�RB�RB�LB�LB�FB�FB�FB�FB�FB�?B�?B�?B�?B�9B�9B�9B�9B�9B�9B�9B�9B�3B�3B�3B�3B�3B�3B�3B�3B�3B�3B�3B�3B�3B�3B�-B�'B�!B�!B�!B�!B�'B�'B�!B�B�B��B�VBcTB�sB��B|�Bv�Bo�BdZBP�BL�BF�B@�B49B(�B�B�BJB	7B�B�fB�)B��BŢB�dB�?B�B��B��B�{B�7B�Bq�BbNB;dB$�BoBB�B�B�mB�5B��B��BŢB�?B�B��B��B��B��B��B�JB�%B�Bz�Bu�Bk�Be`BZBR�BI�B@�BC�BB�B:^B33B.B,B#�B!�B�B�B�B�BhBVBB��B��B��B��B��B�B�yB�ZB�BB�)B�B��B��B��B��B��B��BȴBǮBƨBƨBÖB��B�LB�'B�B��B��B��B��B��B��B��B�{B�oB�bB�VB�=B�1B�+B|�By�Bw�Bv�Bu�Bt�Br�Bo�Bk�BhsBgmBe`BcTB_;BaHBbNB`BB^5BYBP�BE�B>wB:^B8RB7LB6FB5?B2-B)�B#�B �B�B�B�B�B�B�B�B�BuB\BVBDBDB
=B
=B1B%BBB
��B
��B
��B
��B
��B
��B
�B
�B
�B
�B
�B
�yB
�mB
�mB
�`B
�`B
�ZB
�NB
�NB
�HB
�BB
�BB
�;B
�5B
�/B
�/B
�#B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
ɺB
ȴB
ǮB
ǮB
ŢB
ĜB
��B
��B
�}B
�wB
�qB
�wB
�jB
�qB
�jB
�jB
�dB
�dB
�dB
�dB
�jB
�dB
�^B
�dB
�dB
�dB
�dB
�^B
�dB
�qB
�jB
�dB
�jB
�dB
�qB
�dB
�wB
�}B
��B
��B
��B
��B
B
B
ÖB
ĜB
ŢB
ȴB
��B
��B
��B
��B
��B
��B
��B
��B
��B
�#B
�/B
�;B
�BB
�`B
�fB
�B
�B
�B
�B
�B
��B
��B
��B
��B
��B
��B  BB	7B
=BDBPBuB�B�B�B �B!�B#�B$�B%�B'�B-B-B0!B49B8RB<jB<jB=qB>wB>wB?}B@�BA�BB�BD�BE�BF�BH�BN�BP�BR�BT�BYB\)B\)B\)B\)B^5B`BBdZBiyBn�Br�Bu�Bx�By�Bz�B{�B|�B~�B�B�B�7B�DB�PB�\B�bB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�3B�9B�FB�FB�RB�jB�qB�}B��BBBŢBȴB��B��B��B��B��B��B��B�
B�B�B�B�B�B�)B�/B�/B�5B�HB�NB�`B�sB�B�B�B�B�B��B��B��B��B��B��B��BBB+B1B	7BJBVBbBuB{B�B�B�B�B�B�B�B �B"�B$�B'�B)�B+B,B.B/B0!B1'B2-B2-B2-B33B33B6FB9XB:^B<jB>wB?}B@�B@�BA�B@�BA�BF�BI�BL�BN�BO�BO�BR�BT�BVBW
BXBYBYBZB[#B\)B`BBcTBffBgmBhsBiyBjBk�Bl�Bl�Bl�Bn�Bp�Bq�Bs�Bt�Bu�Bx�Bz�B{�B|�B}�B~�B~�B~�B~�B�B�B�B�B�B�%B�%B�+B�1B�7B�DB�PB�PB�PB�VB�\B�hB�oB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�!B�'B�'B�'B�3B�9B�FB�FB�LB�XB�^B�dB�jB�jB�qB�wB�}B��B��BBBBĜBĜBƨBǮBǮBȴBɺB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�
B�
B�B�B�B�B�B�#B�#B�#B�)B�)B�/B�/B�5B�;B�;B�BB�HB�NB�TB�TB�ZB�ZB�`B�fB�fB�mB�fB�mB�mB�sB�yB�yB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B  B  BBBBBBBBBBBBBBBBBBB+B+B+B1B1B	7B	7B	7B	7BDBDBDBDBJBPBPBVB\B\B\B\B\B\BbBbBhBhBhBhBoBoBoBoBuBuBuB{B{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B �B!�B!�B"�B"�B"�B"�B"�B#�B#�B#�B#�B$�B$�B$�B$�B%�B%�B%�B&�B&�B'�B'�B(�B(�B)�B)�B+B+B+B,B,B,B-B-B-B.B.B/B/B0!B0!B0!B1'B1'B1'B1'B1'B1'B2-B2-B33B33B33B49B49B5?B5?B5?B5?B5?B6FB6FB7LB7LB8RB8RB8RB9XB9XB9XB9XB:^B:^B:^B:^B:^B:^B:^B;dB:^B:^B;dB;dB<jB;dB;dB<jB<jB<jB<jB<jB<jB=qB=qB=qB>wB>wB>wB>wB?}B?}B?}B@�B@�B@�B@�B@�BA�BA�BA�BA�BA�BB�BB�BB�BC�BC�BC�BC�BD�BD�BD�BD�BE�BE�BD�BE�BE�BE�BE�BF�BF�BF�BF�BG�BG�BG�BG�BG�BG�BG�BH�BH�BH�BH�BI�BI�BI�BI�BJ�BJ�BJ�BK�BK�BK�BK�BK�BL�BL�BL�BL�BL�BM�BM�BM�BM�BN�BN�BN�BN�BN�BN�BN�BO�BO�BO�BO�BP�BP�BP�BP�BQ�BQ�BQ�BQ�BQ�BR�BR�BR�BR�BR�BS�BS�BS�BS�BT�BVBVBVBVBV444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  B�RB�RB�RB�LB�LB�FB�FB�FB�FB�FB�?B�?B�?B�?B�9B�9B�9B�9B�9B�9B�9B�9B�3B�3B�3B�3B�3B�3B�3B�3B�3B�3B�3B�3B�3B�3B�-B�'B�!B�!B�!B�!B�'B�'B�!B�B�B��B�VBcTB�sB��B|�Bv�Bo�BdZBP�BL�BF�B@�B49B(�B�B�BJB	7B�B�fB�)B��BŢB�dB�?B�B��B��B�{B�7B�Bq�BbNB;dB$�BoBB�B�B�mB�5B��B��BŢB�?B�B��B��B��B��B��B�JB�%B�Bz�Bu�Bk�Be`BZBR�BI�B@�BC�BB�B:^B33B.B,B#�B!�B�B�B�B�BhBVBB��B��B��B��B��B�B�yB�ZB�BB�)B�B��B��B��B��B��B��BȴBǮBƨBƨBÖB��B�LB�'B�B��B��B��B��B��B��B��B�{B�oB�bB�VB�=B�1B�+B|�By�Bw�Bv�Bu�Bt�Br�Bo�Bk�BhsBgmBe`BcTB_;BaHBbNB`BB^5BYBP�BE�B>wB:^B8RB7LB6FB5?B2-B)�B#�B �B�B�B�B�B�B�B�B�BuB\BVBDBDB
=B
=B1B%BBB
��B
��B
��B
��B
��B
��B
�B
�B
�B
�B
�B
�yB
�mB
�mB
�`B
�`B
�ZB
�NB
�NB
�HB
�BB
�BB
�;B
�5B
�/B
�/B
�#B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
ɺB
ȴB
ǮB
ǮB
ŢB
ĜB
��B
��B
�}B
�wB
�qB
�wB
�jB
�qB
�jB
�jB
�dB
�dB
�dB
�dB
�jB
�dB
�^B
�dB
�dB
�dB
�dB
�^B
�dB
�qB
�jB
�dB
�jB
�dB
�qB
�dB
�wB
�}B
��B
��B
��B
��B
B
B
ÖB
ĜB
ŢB
ȴB
��B
��B
��B
��B
��B
��B
��B
��B
��B
�#B
�/B
�;B
�BB
�`B
�fB
�B
�B
�B
�B
�B
��B
��B
��B
��B
��B
��B  BB	7B
=BDBPBuB�B�B�B �B!�B#�B$�B%�B'�B-B-B0!B49B8RB<jB<jB=qB>wB>wB?}B@�BA�BB�BD�BE�BF�BH�BN�BP�BR�BT�BYB\)B\)B\)B\)B^5B`BBdZBiyBn�Br�Bu�Bx�By�Bz�B{�B|�B~�B�B�B�7B�DB�PB�\B�bB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�3B�9B�FB�FB�RB�jB�qB�}B��BBBŢBȴB��B��B��B��B��B��B��B�
B�B�B�B�B�B�)B�/B�/B�5B�HB�NB�`B�sB�B�B�B�B�B��B��B��B��B��B��B��BBB+B1B	7BJBVBbBuB{B�B�B�B�B�B�B�B �B"�B$�B'�B)�B+B,B.B/B0!B1'B2-B2-B2-B33B33B6FB9XB:^B<jB>wB?}B@�B@�BA�B@�BA�BF�BI�BL�BN�BO�BO�BR�BT�BVBW
BXBYBYBZB[#B\)B`BBcTBffBgmBhsBiyBjBk�Bl�Bl�Bl�Bn�Bp�Bq�Bs�Bt�Bu�Bx�Bz�B{�B|�B}�B~�B~�B~�B~�B�B�B�B�B�B�%B�%B�+B�1B�7B�DB�PB�PB�PB�VB�\B�hB�oB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�!B�'B�'B�'B�3B�9B�FB�FB�LB�XB�^B�dB�jB�jB�qB�wB�}B��B��BBBBĜBĜBƨBǮBǮBȴBɺB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�
B�
B�B�B�B�B�B�#B�#B�#B�)B�)B�/B�/B�5B�;B�;B�BB�HB�NB�TB�TB�ZB�ZB�`B�fB�fB�mB�fB�mB�mB�sB�yB�yB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B  B  BBBBBBBBBBBBBBBBBBB+B+B+B1B1B	7B	7B	7B	7BDBDBDBDBJBPBPBVB\B\B\B\B\B\BbBbBhBhBhBhBoBoBoBoBuBuBuB{B{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B �B!�B!�B"�B"�B"�B"�B"�B#�B#�B#�B#�B$�B$�B$�B$�B%�B%�B%�B&�B&�B'�B'�B(�B(�B)�B)�B+B+B+B,B,B,B-B-B-B.B.B/B/B0!B0!B0!B1'B1'B1'B1'B1'B1'B2-B2-B33B33B33B49B49B5?B5?B5?B5?B5?B6FB6FB7LB7LB8RB8RB8RB9XB9XB9XB9XB:^B:^B:^B:^B:^B:^B:^B;dB:^B:^B;dB;dB<jB;dB;dB<jB<jB<jB<jB<jB<jB=qB=qB=qB>wB>wB>wB>wB?}B?}B?}B@�B@�B@�B@�B@�BA�BA�BA�BA�BA�BB�BB�BB�BC�BC�BC�BC�BD�BD�BD�BD�BE�BE�BD�BE�BE�BE�BE�BF�BF�BF�BF�BG�BG�BG�BG�BG�BG�BG�BH�BH�BH�BH�BI�BI�BI�BI�BJ�BJ�BJ�BK�BK�BK�BK�BK�BL�BL�BL�BL�BL�BM�BM�BM�BM�BN�BN�BN�BN�BN�BN�BN�BO�BO�BO�BO�BP�BP�BP�BP�BQ�BQ�BQ�BQ�BQ�BR�BR�BR�BR�BR�BS�BS�BS�BS�BT�BVBVBVBVBV444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.22 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20220124080111                              AO  ARCAADJP                                                                    20220124080111    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20220124080111  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20220124080111  QCF$                G�O�G�O�G�O�8000            