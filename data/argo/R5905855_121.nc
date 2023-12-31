CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-09T18:49:01Z creation;2022-06-09T18:49:01Z conversion to V3.1      
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
_FillValue                 �  I@   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M0   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �P   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �@   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220609184901  20220609185840  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               yA   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @�ՔB.E1   @�Ք���@0��G�{�c��t�j1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @333@�33@�  A   AffA@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B���B���B���B�  B�  B���B���B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33C 33C��C�fC  C�fC
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,�C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^�C`�Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@%�@xQ�@���@���A�GA<z�A\z�A|z�A�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qB�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B��\B�(�B�\)B�(�B��\B��\B�(�B�(�B��\B��\B��\B��\B�(�B�\)B��\B��\BÏ\BǏ\Bˏ\BϏ\Bӏ\B׏\Bۏ\Bߏ\B�\B�\B�\B�\B�\B��\B�B���C�{C�CǮC�C	ǮCǮCǮCǮCǮCǮCǮCǮCǮCǮCǮCǮC!ǮC#ǮC%ǮC'ǮC)ǮC+�HC-ǮC/ǮC1ǮC3ǮC5ǮC7ǮC9ǮC;ǮC=ǮC?ǮCAǮCCǮCEǮCGǮCIǮCKǮCMǮCOǮCQǮCSǮCUǮCWǮCYǮC[ǮC]�HC_�HCaǮCcǮCeǮCgǮCiǮCkǮCmǮCoǮCqǮCsǮCuǮCwǮCyǮC{ǮC}ǮCǮC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D q�D ��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��D	q�D	��D
q�D
��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��D q�D ��D!q�D!��D"q�D"��D#q�D#��D$q�D$��D%q�D%��D&q�D&��D'q�D'��D(q�D(��D)q�D)��D*q�D*��D+q�D+��D,q�D,��D-q�D-��D.q�D.��D/q�D/��D0q�D0��D1q�D1��D2q�D2��D3q�D3��D4q�D4��D5q�D5��D6q�D6��D7q�D7��D8q�D8��D9q�D9��D:q�D:��D;q�D;��D<q�D<��D=q�D=��D>q�D>��D?q�D?��D@q�D@��DAq�DA��DBq�DB��DCq�DC��DDq�DD��DEq�DE��DFq�DF��DGq�DG��DHq�DH��DIq�DI��DJq�DJ��DKq�DK��DLq�DL��DMq�DM��DNq�DN��DOq�DO��DPq�DP��DQq�DQ��DRq�DR��DSq�DS��DTq�DT��DUq�DU��DVq�DV��DWq�DW��DXq�DX��DYq�DY��DZq�DZ��D[q�D[��D\q�D\��D]q�D]��D^q�D^��D_q�D_��D`q�D`��Daq�Da��Dbq�Db��Dcq�Dc��Ddq�Dd��Deq�De��Dfq�Df��Dgq�Dg��Dhq�Dh��Diq�Di��Djq�Dj��Dkq�Dk��Dlq�Dl��Dmq�Dm��Dnq�Dn��Doq�Do��Dpq�Dp��Dqq�Dq��Drq�Dr��Dsq�Ds��Dtq�Dt��Duq�Du��Dvq�Dv��Dwq�Dw��Dxq�Dx��Dyq�Dy��Dzq�Dz��D{q�D{��D|q�D|��D}q�D}��D~q�D~��Dq�D��D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�u�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D��)D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D¸�D���D�8�D�x�Dø�D���D�8�D�x�Dĸ�D���D�8�D�x�DŸ�D���D�8�D�x�DƸ�D���D�8�D�x�DǸ�D���D�8�D�x�Dȸ�D���D�8�D�x�Dɸ�D���D�8�D�x�Dʸ�D���D�8�D�x�D˸�D���D�8�D�x�D̸�D���D�8�D�x�D͸�D���D�8�D�x�Dθ�D���D�8�D�x�Dϸ�D���D�8�D�x�Dи�D���D�8�D�x�DѸ�D���D�8�D�x�DҸ�D���D�8�D�x�DӸ�D���D�8�D�x�DԸ�D���D�8�D�x�Dո�D���D�8�D�x�Dָ�D���D�8�D�x�D׸�D���D�8�D�x�Dظ�D���D�8�D�x�Dٸ�D���D�8�D�x�Dڸ�D���D�8�D�x�D۸�D���D�8�D�x�Dܸ�D���D�8�D�x�Dݸ�D���D�8�D�x�D޸�D���D�8�D�x�D߸�D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D���D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�}�A��Aσ�Aς�AυSAψ�AφYA�{�A�a�A��8A΅�A�|�A�tA�poA�e�A�_pA�T�A�3�A��A��%A��AʹnAͰ�A͈fA�6�A��A�A���A��A��[A̮}A�z�A�P�A�<6Aˢ4A�ѷAɏ(AǞA�u�A�"hA�FtA�.�A��&A�ݘA�qAA��A��'A��<A�(�A�:A��ZA���A���A�A�A���A���A�?�A��7A�%zA�{A�<6A��uA�l�A�)�A�a�A���A��	A�'�A�kA��A���A�'RA���A��FA�y�A�ԕA�h�A�ϫA��LA�l�A�>�A���A��)A�]dA�	lA}b�Az�Av�At�Aq�Ao�LAi��Ada�Aa0�A_sA]Q�A\8�A[�wA[xATcAR��AQ	AL��AH�AEw2AB��A=1A;	A97LA7�A5�3A4�A1�eA1�A0%�A/�A.c A-��A-5�A-�A,��A+T�A*�A)��A)A(�PA(�,A'?A'��A'~(A$O�A"��A!�oA!f�A �A�FA�gA	A֡A��A��A xA e,A ��A W?A FtA @�A 9�A  �A��APHA�A��A��A_A:*A	A
=A��A\�A�}An�A��A�A�WA��Ab�A#:A�KA/�A�|A\�AsAP�A�rA�EA��Aa�A�A�A~�A`BA`BA>BA�A��AhsA!�A�qAm�A �A�aA{�A-A�AVmAa�A�rA/�A��AtTA�A�kAd�A�A��A/�A!A
��A
��A
��A
^5A	u�A	E9A	eA�A�<A��AQA�4A?}A+A;A��A�A��AJ#A�A�bA=�A��A4Az�AAFtA �dA xlA *0@�:�@�(�@��5@�?�@���@�.I@��@���@�"�@�^5@��	@�,�@� i@�a|@��H@�)�@�
=@�@��8@��@��@�kQ@��#@�@�4@�-@�A�@���@���@�y>@�ϫ@�}@�=@�x@�F�@�L�@��K@�YK@��@癚@�k�@�G�@�C�@�2a@�+@�=@䍹@���@��@�ԕ@�=@���@�@�(�@�L0@ݎ"@܉�@���@�J�@ڮ}@�^5@�Dg@�_�@���@ש*@�F@�~(@�%�@��)@ձ[@�qv@Թ$@�U2@��@Ӆ@Ұ!@ќ�@�>�@�C�@�bN@�J#@α�@�1�@͛=@�k�@�/�@�\�@ˌ~@��@���@ʇ+@�6�@�~�@�V@ȑ @Ȍ@�8�@��@�x�@�G�@�7L@�#�@Ƒ�@��@ų�@�{J@�W?@�+�@��@Ĺ$@�1�@�@��r@���@��A@Ê	@��@©�@��@��@�^5@��@���@��@�G�@��@�z�@�z@�?�@��[@�hs@�f�@��@���@�͟@���@�'R@��Q@���@�Vm@�=@���@��@��Z@���@�iD@�@O@��@�kQ@���@��=@�p�@���@���@���@��Q@��7@�C@�
�@�iD@�(@��@���@��h@��1@�s�@�/�@�?}@���@���@�ȴ@���@���@��j@��j@���@��@�:*@�1@��a@�s@��@�ں@��@�j@�!@�خ@���@�O�@��@��8@���@�.�@�  @���@���@���@�J#@���@�A�@�@�o @�.I@��@�h�@��@@�U�@��@���@�@��@���@��f@�=�@��@���@���@�n�@�V�@�;�@�!@���@���@��-@��@�<�@��@��@�B�@��@���@�~�@�	@���@��X@�L�@�Y@�o@��8@�y>@�Ta@�!@���@��{@�2a@�S@���@���@�YK@� �@��@��-@��	@�W?@��@�͟@���@���@�ff@�V�@�@���@�\�@��@��v@���@��_@�a|@�G@���@�Y�@�@���@�Ĝ@�v�@�1�@��Z@���@��h@�[W@�Mj@�F�@�@O@�5�@�+�@��@��|@�� @�R�@��t@�e,@�(�@��s@��r@�M�@��@��6@��~@�4�@���@�m�@�Z�@�N�@�&�@���@���@�_p@�33@�%@��R@��@���@��o@�~�@�tT@�_�@�&�@��@��o@��9@�(�@�z@�!@���@��@��3@���@�{J@�0�@��U@�Z�@��@��@�p�@��M@��!@��z@�oi@��@��@��w@���@���@�g�@�:�@��?@�p;@��W@�IR@��U@��@���@�c�@�-�@�@�_@��r@���@�t�@�Vm@�A�@�%F@���@�a|@�ƨ@���@�c@�x�@�m]@�q@��v@���@�e@{J@j�@X�@.I@~��@~J�@}�t@}G�@}q@|��@|�9@|�@|�@|�4@|��@|�u@|�u@|~(@|[�@|m�@|*�@{خ@{6z@zں@zu%@y��@x�)@w�@w��@w�*@w�{@w;d@v�]@v�@vGE@u��@u�X@uS&@t��@s��@r�"@r�'@r8�@q��@qB�@p�@p�?@p�Y@p*�@o�6@o��@og�@oC@n�L@m�C@l�@ltT@k�}@jTa@i�^@iT�@h�Y@h$@hb@g�W@g�w@g��@g�@f�@f� @fl�@e��@ef�@e�@d��@d~(@c�}@c>�@b�2@b��@b~�@b:*@b�@a�D@a��@a�)@a�9@a��@a��@au�@aT�@a�@`�@_@O@_�@^�s@^��@^p;@^GE@^{@]��@]�9@]�t@]�@]B�@]�@\K^@[��@[�g@[�@[�[@[,�@Z�}@Z~�@Y�d@YL�@X֡@X�4@X6@W��@WX�@W
=@V�@V�@Uw2@U@T��@TtT@T�@Sn/@S=@S�@R��@RO@Q�@P�	@P֡@P�U@P��@PG@O�k@O\)@O(@N�8@N�h@N{�@N5?@M�T@MT�@M%@L��@L �@K�r@KiD@J͟@J��@J��@I@I7L@H�@Hh�@Gخ@G�	@G�@F�@F��@F�@FR�@E��@EB�@D��@D~@C�@C�k@Cqv@CJ#@C@O@C,�@C�@B�m@B��@BV@B�@A�d@A�@Azx@A/@@�j@@$@@�@@�@@�@?W?@?E9@>��@>	@=��@<��@<�O@<?�@<@<�@;�]@;˒@;��@;4�@:�X@:s�@:)�@:_@9�9@9��@9!�@8�@87�@7��@7��@7�*@7�@6�6@6q�@5ϫ@5��@5��@5|@5w2@5rG@5o @5(�@4��@4�O@4�9@4�O@4�u@4~(@4[�@3�@3�@3K�@3�@2�,@2�6@23�@1�o@1�@1��@1c�@1N<@1	l@0�e@0[�@0�@/��@/��@/P�@.�"@.��@.��@.�@-�@-��@-�~@-u�@-B�@-/@,�f@,�?@,��@,�.@+�@*�y@*��@*c @*
�@)�d@)��@)o @)#�@(Ĝ@(@'�g@'�[@'�f@'.I@&�M@&	@%a�@$�P@$��@$�@$Q�@$7@#�A@#�F@#�@#@O@"�]@"��@"��@"R�@"#:@!��@!��@!��@!^�@ �@ -�@��@8@��@�}@��@M�@�@��@4@bN@ݘ@C�@&@S@��@�m@��@ȴ@�h@�6@��@��@}V@_�@L0@@�@!�@��@�@�=@c�@X@X@Vm@T�@T�@S&@O�@:�@�?@z�@S�@C-@�@��@t�@g�@P�@@O@>�@;d@6z@/�@&@"�@�@o@S@�M@ߤ@��@��@��@�r@�A@R�@�@�-@��@��@�@�7@j@?}@��@�j@�z@��@r�@Ft@<�@-�@�@M@  @ݘ@�$@��@E9@"�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�}�A��Aσ�Aς�AυSAψ�AφYA�{�A�a�A��8A΅�A�|�A�tA�poA�e�A�_pA�T�A�3�A��A��%A��AʹnAͰ�A͈fA�6�A��A�A���A��A��[A̮}A�z�A�P�A�<6Aˢ4A�ѷAɏ(AǞA�u�A�"hA�FtA�.�A��&A�ݘA�qAA��A��'A��<A�(�A�:A��ZA���A���A�A�A���A���A�?�A��7A�%zA�{A�<6A��uA�l�A�)�A�a�A���A��	A�'�A�kA��A���A�'RA���A��FA�y�A�ԕA�h�A�ϫA��LA�l�A�>�A���A��)A�]dA�	lA}b�Az�Av�At�Aq�Ao�LAi��Ada�Aa0�A_sA]Q�A\8�A[�wA[xATcAR��AQ	AL��AH�AEw2AB��A=1A;	A97LA7�A5�3A4�A1�eA1�A0%�A/�A.c A-��A-5�A-�A,��A+T�A*�A)��A)A(�PA(�,A'?A'��A'~(A$O�A"��A!�oA!f�A �A�FA�gA	A֡A��A��A xA e,A ��A W?A FtA @�A 9�A  �A��APHA�A��A��A_A:*A	A
=A��A\�A�}An�A��A�A�WA��Ab�A#:A�KA/�A�|A\�AsAP�A�rA�EA��Aa�A�A�A~�A`BA`BA>BA�A��AhsA!�A�qAm�A �A�aA{�A-A�AVmAa�A�rA/�A��AtTA�A�kAd�A�A��A/�A!A
��A
��A
��A
^5A	u�A	E9A	eA�A�<A��AQA�4A?}A+A;A��A�A��AJ#A�A�bA=�A��A4Az�AAFtA �dA xlA *0@�:�@�(�@��5@�?�@���@�.I@��@���@�"�@�^5@��	@�,�@� i@�a|@��H@�)�@�
=@�@��8@��@��@�kQ@��#@�@�4@�-@�A�@���@���@�y>@�ϫ@�}@�=@�x@�F�@�L�@��K@�YK@��@癚@�k�@�G�@�C�@�2a@�+@�=@䍹@���@��@�ԕ@�=@���@�@�(�@�L0@ݎ"@܉�@���@�J�@ڮ}@�^5@�Dg@�_�@���@ש*@�F@�~(@�%�@��)@ձ[@�qv@Թ$@�U2@��@Ӆ@Ұ!@ќ�@�>�@�C�@�bN@�J#@α�@�1�@͛=@�k�@�/�@�\�@ˌ~@��@���@ʇ+@�6�@�~�@�V@ȑ @Ȍ@�8�@��@�x�@�G�@�7L@�#�@Ƒ�@��@ų�@�{J@�W?@�+�@��@Ĺ$@�1�@�@��r@���@��A@Ê	@��@©�@��@��@�^5@��@���@��@�G�@��@�z�@�z@�?�@��[@�hs@�f�@��@���@�͟@���@�'R@��Q@���@�Vm@�=@���@��@��Z@���@�iD@�@O@��@�kQ@���@��=@�p�@���@���@���@��Q@��7@�C@�
�@�iD@�(@��@���@��h@��1@�s�@�/�@�?}@���@���@�ȴ@���@���@��j@��j@���@��@�:*@�1@��a@�s@��@�ں@��@�j@�!@�خ@���@�O�@��@��8@���@�.�@�  @���@���@���@�J#@���@�A�@�@�o @�.I@��@�h�@��@@�U�@��@���@�@��@���@��f@�=�@��@���@���@�n�@�V�@�;�@�!@���@���@��-@��@�<�@��@��@�B�@��@���@�~�@�	@���@��X@�L�@�Y@�o@��8@�y>@�Ta@�!@���@��{@�2a@�S@���@���@�YK@� �@��@��-@��	@�W?@��@�͟@���@���@�ff@�V�@�@���@�\�@��@��v@���@��_@�a|@�G@���@�Y�@�@���@�Ĝ@�v�@�1�@��Z@���@��h@�[W@�Mj@�F�@�@O@�5�@�+�@��@��|@�� @�R�@��t@�e,@�(�@��s@��r@�M�@��@��6@��~@�4�@���@�m�@�Z�@�N�@�&�@���@���@�_p@�33@�%@��R@��@���@��o@�~�@�tT@�_�@�&�@��@��o@��9@�(�@�z@�!@���@��@��3@���@�{J@�0�@��U@�Z�@��@��@�p�@��M@��!@��z@�oi@��@��@��w@���@���@�g�@�:�@��?@�p;@��W@�IR@��U@��@���@�c�@�-�@�@�_@��r@���@�t�@�Vm@�A�@�%F@���@�a|@�ƨ@���@�c@�x�@�m]@�q@��v@���@�e@{J@j�@X�@.I@~��@~J�@}�t@}G�@}q@|��@|�9@|�@|�@|�4@|��@|�u@|�u@|~(@|[�@|m�@|*�@{خ@{6z@zں@zu%@y��@x�)@w�@w��@w�*@w�{@w;d@v�]@v�@vGE@u��@u�X@uS&@t��@s��@r�"@r�'@r8�@q��@qB�@p�@p�?@p�Y@p*�@o�6@o��@og�@oC@n�L@m�C@l�@ltT@k�}@jTa@i�^@iT�@h�Y@h$@hb@g�W@g�w@g��@g�@f�@f� @fl�@e��@ef�@e�@d��@d~(@c�}@c>�@b�2@b��@b~�@b:*@b�@a�D@a��@a�)@a�9@a��@a��@au�@aT�@a�@`�@_@O@_�@^�s@^��@^p;@^GE@^{@]��@]�9@]�t@]�@]B�@]�@\K^@[��@[�g@[�@[�[@[,�@Z�}@Z~�@Y�d@YL�@X֡@X�4@X6@W��@WX�@W
=@V�@V�@Uw2@U@T��@TtT@T�@Sn/@S=@S�@R��@RO@Q�@P�	@P֡@P�U@P��@PG@O�k@O\)@O(@N�8@N�h@N{�@N5?@M�T@MT�@M%@L��@L �@K�r@KiD@J͟@J��@J��@I@I7L@H�@Hh�@Gخ@G�	@G�@F�@F��@F�@FR�@E��@EB�@D��@D~@C�@C�k@Cqv@CJ#@C@O@C,�@C�@B�m@B��@BV@B�@A�d@A�@Azx@A/@@�j@@$@@�@@�@@�@?W?@?E9@>��@>	@=��@<��@<�O@<?�@<@<�@;�]@;˒@;��@;4�@:�X@:s�@:)�@:_@9�9@9��@9!�@8�@87�@7��@7��@7�*@7�@6�6@6q�@5ϫ@5��@5��@5|@5w2@5rG@5o @5(�@4��@4�O@4�9@4�O@4�u@4~(@4[�@3�@3�@3K�@3�@2�,@2�6@23�@1�o@1�@1��@1c�@1N<@1	l@0�e@0[�@0�@/��@/��@/P�@.�"@.��@.��@.�@-�@-��@-�~@-u�@-B�@-/@,�f@,�?@,��@,�.@+�@*�y@*��@*c @*
�@)�d@)��@)o @)#�@(Ĝ@(@'�g@'�[@'�f@'.I@&�M@&	@%a�@$�P@$��@$�@$Q�@$7@#�A@#�F@#�@#@O@"�]@"��@"��@"R�@"#:@!��@!��@!��@!^�@ �@ -�@��@8@��@�}@��@M�@�@��@4@bN@ݘ@C�@&@S@��@�m@��@ȴ@�h@�6@��@��@}V@_�@L0@@�@!�@��@�@�=@c�@X@X@Vm@T�@T�@S&@O�@:�@�?@z�@S�@C-@�@��@t�@g�@P�@@O@>�@;d@6z@/�@&@"�@�@o@S@�M@ߤ@��@��@��@�r@�A@R�@�@�-@��@��@�@�7@j@?}@��@�j@�z@��@r�@Ft@<�@-�@�@M@  @ݘ@�$@��@E9@"�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	YB	YB	ZkB	YKB	Y�B	[�B	\CB	^B	cTB	qAB	raB	r�B	r-B	rB	rGB	r�B	s3B	r|B	q�B	q'B	r�B	r-B	q�B	q'B	t�B	vzB	x�B	z�B	{�B	|�B	~�B	�[B	�9B	��B	�MB	�hB	��B
B
�|B
�wBh�B��B�hB�3B��B��B�!B�lB�B vBB�B0B&�B�kB��B�9B��B��B�B�B�_ByrBE�B+kB�B
�tB
�WB
��B
��B
��B
� B
��B
�B
��B
m�B
c B
T�B
DMB
5�B
+�B
%,B
dB
�B	��B	��B	�B	�gB	��B	��B	�B	�B	g�B	X�B	QB	H1B	BAB	>(B	6FB	$tB	B	�B��B�MB��B�TB��BΥB�KB��B�dB��B�WB�QB�kB��B�DB��B��B�XB��B��B�!B��B��B��B��B�@B	�B	&�B	 �B	�B	�B	#�B	(XB	)�B	'�B	%`B	,=B	A;B	\�B	w�B	�B	�.B	��B	��B	�-B	��B	�`B	�B	�IB	�|B	�ZB	�*B	��B	�B	��B	�B	�B	҉B	یB	�FB	�B	�nB	��B	�wB
oB
3B
MB
B
�B
'B	�VB	�wB
1B
�B
9B
�B
zB
�B

=B

#B

#B

rB

rB

	B
�B
B
B
�B
xB
^B
�B
�B
�B
�B
�B
WB
�B
�B
qB
�B
qB
�B
�B
�B
�B
kB
�B
�B
�B
�B
�B
�B
�B
=B
�B
QB
�B
B
�B
�B
�B
_B
�B
_B
_B
sB
9B
�B
�B
�B
oB
 B
HB
(B
B
�B
~B
xB
)B

�B
	�B
	B
KB
�B
�B
�B
�B
�B
�B
�B	��B	�"B	�B	��B	��B
 OB
 B
  B	�cB	��B	��B
B	��B	�VB	�jB	��B	��B	��B	��B	�>B	�*B	��B	��B	��B	��B	��B	�0B	��B	��B	��B	��B	�JB	�$B	��B	��B	�+B	��B	��B	�ZB	��B	��B	��B	�B	�B	�B	�B	�aB	�B	�TB	�B	�2B	��B	��B	��B	�2B	��B	�RB	�B	��B	��B	�	B	��B	�B	��B	�2B	�LB	�%B	��B	��B	�B	�B	�FB	�8B	�XB	��B	��B	�rB	�$B	��B	�B	�B	��B	��B	��B	��B	�xB	�xB	�^B	��B	�jB	��B	��B	�B	�dB	�B	��B	��B	��B	��B	��B	��B	�.B	�}B	�B	�HB	��B
 B
 4B
 B	��B
  B
 �B
B
 B
�B
UB
 �B
'B
�B
B
�B
AB
�B
�B
�B
�B
�B
{B
�B
B
�B
�B
[B
uB
B
�B
�B
�B
?B
tB
YB
�B
zB
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
	�B
xB
~B
PB
jB
�B
�B
�B
�B
B
�B
�B
�B
(B
�B
�B
�B
�B
HB
HB
bB
}B
bB
HB
�B
�B
�B
�B
�B
�B
 B
�B
TB
:B
B
�B
�B
�B
uB
[B
@B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
MB
gB
gB
MB
�B
SB

B
�B
�B
B
_B
yB
KB
eB
KB
�B
�B
�B
�B
kB
7B
QB
�B
�B
�B
�B
	B
WB
qB
�B
�B
)B
]B
xB
B
IB
~B
~B
�B
�B
B
�B
�B
!B
�B
VB
VB
�B
 B
 vB
 �B
!-B
!-B
!HB
!|B
!�B
"B
"NB
"�B
"�B
"�B
"�B
#B
#B
"�B
"�B
# B
#nB
#�B
$B
$B
$@B
$�B
%�B
&B
&fB
&�B
&�B
'8B
'�B
(
B
($B
(
B
(>B
(XB
(�B
(�B
(�B
)B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
*B
*B
)�B
)�B
*�B
+�B
,"B
,=B
,"B
,WB
,=B
,=B
,�B
-B
-�B
-�B
-wB
.IB
.�B
/5B
/ B
/�B
/�B
/�B
0UB
0!B
0UB
0;B
0;B
0�B
0!B
/�B
.�B
.�B
.cB
.cB
.�B
.�B
.�B
.�B
.�B
.�B
/OB
/5B
/5B
/5B
/iB
/�B
0�B
0�B
0�B
0�B
0�B
1B
1AB
1�B
2aB
2�B
2�B
2�B
2�B
3MB
3�B
3�B
4B
4B
4TB
4�B
4�B
4�B
4�B
4�B
4�B
4�B
4�B
7B
8�B
9	B
9$B
9XB
9XB
9XB
9rB
9�B
:*B
:*B
9�B
:B
:^B
:DB
:*B
:xB
:�B
:�B
:�B
:�B
;0B
;dB
;�B
<�B
<�B
="B
=<B
=VB
=�B
=�B
>(B
>BB
>(B
>B
>B
>]B
>]B
>B
>]B
?�B
@ B
@iB
A B
AUB
AUB
AoB
AoB
AUB
BB
A�B
A�B
A�B
BAB
BuB
B�B
B�B
CB
C�B
DMB
DgB
D�B
D�B
EB
ESB
ESB
E9B
ESB
EmB
ESB
EmB
E�B
E�B
E�B
FYB
F�B
F�B
F�B
F�B
GB
G+B
GB
F�B
F�B
F�B
F�B
F�B
F�B
G_B
G�B
HKB
H1B
HB
HfB
H�B
H�B
I7B
IRB
I�B
I�B
JrB
J�B
KB
J�B
J�B
J�B
J�B
K)B
KDB
K^B
K�B
K�B
L�B
M�B
M�B
M6B
L�B
K�B
LB
LB
K�B
K�B
LB
L0B
LdB
LJB
LdB
LdB
L�B
MB
MB
MB
N"B
NpB
NpB
O(B
O�B
P}B
P�B
Q B
QB
QhB
RoB
RoB
R�B
R�B
R�B
R�B
R�B
R�B
S&B
S�B
TaB
UB
U�B
U�B
V9B
V�B
VmB
V�B
V�B
V�B
V�B
V�B
W$B
W$B
W?B
W?B
W?B
WYB
WsB
W�B
W�B
WYB
XB
XB
X�B
X�B
X�B
YeB
YB
ZB
Z7B
Z7B
ZQB
ZkB
ZkB
Z�B
Z�B
Z�B
Z�B
Z�B
ZQB
Z7B
Z7B
Z�B
[#B
[WB
[�B
]�B
]�B
]�B
]�B
^B
]�B
]�B
]�B
]�B
]�B
]�B
]�B
]�B
]�B
]�B
]�B
]�B
]�B
]�B
]~B
]�B
]�B
]�B
]�B
]�B
^�B
^�B
^jB
^�B
^�B
^�B
_;B
_;B
_�B
_�B
`BB
`BB
`\B
`�B
`�B
`�B
abB
a|B
a�B
a�B
a�B
a�B
a�B
a�B
a�B
a�B
a�B
bhB
c:B
c B
c:B
c�B
c�B
c�B
c�B
c�B
dZB
e`B
ezB
e�B
e�B
e�B
e�B
gB
g�B
g�B
hXB
h�B
h�B
h�B
h�B
iDB
i_B
i�B
i�B
j0B
jeB
j�B
j�B
j�B
kB
kQB
kQB
k�B
lWB
l�B
m)B
m�B
m�B
m�B
m�B
ncB
nIB
n�B
oiB
pB
p�B
p�B
p�B
qB
qB
qB
qB
q'B
q'B
q'B
qAB
qAB
q[B
qvB
qvB
q�B
q�B
q�B
q�B
r-B
r-B
rB
rB
rB
rB
q�B
q�B
q�B
r�B
r�B
sB
r�B
s3B
s�B
s�B
s�B
s�B
tB
tB
tB
tB
tB
tB
tB
tB
t9B
t9B
tTB
tnB
t�B
t�B
t�B
t�B
t�B
t�B
utB
u�B
u�B
u�B
u�B
u�B
u�B
u�B
vB
vFB
v`B
vFB
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
wLB
w2B
w�B
w�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	YB	YB	ZkB	YKB	Y�B	[�B	\CB	^B	cTB	qAB	raB	r�B	r-B	rB	rGB	r�B	s3B	r|B	q�B	q'B	r�B	r-B	q�B	q'B	t�B	vzB	x�B	z�B	{�B	|�B	~�B	�[B	�9B	��B	�MB	�hB	��B
B
�|B
�wBh�B��B�hB�3B��B��B�!B�lB�B vBB�B0B&�B�kB��B�9B��B��B�B�B�_ByrBE�B+kB�B
�tB
�WB
��B
��B
��B
� B
��B
�B
��B
m�B
c B
T�B
DMB
5�B
+�B
%,B
dB
�B	��B	��B	�B	�gB	��B	��B	�B	�B	g�B	X�B	QB	H1B	BAB	>(B	6FB	$tB	B	�B��B�MB��B�TB��BΥB�KB��B�dB��B�WB�QB�kB��B�DB��B��B�XB��B��B�!B��B��B��B��B�@B	�B	&�B	 �B	�B	�B	#�B	(XB	)�B	'�B	%`B	,=B	A;B	\�B	w�B	�B	�.B	��B	��B	�-B	��B	�`B	�B	�IB	�|B	�ZB	�*B	��B	�B	��B	�B	�B	҉B	یB	�FB	�B	�nB	��B	�wB
oB
3B
MB
B
�B
'B	�VB	�wB
1B
�B
9B
�B
zB
�B

=B

#B

#B

rB

rB

	B
�B
B
B
�B
xB
^B
�B
�B
�B
�B
�B
WB
�B
�B
qB
�B
qB
�B
�B
�B
�B
kB
�B
�B
�B
�B
�B
�B
�B
=B
�B
QB
�B
B
�B
�B
�B
_B
�B
_B
_B
sB
9B
�B
�B
�B
oB
 B
HB
(B
B
�B
~B
xB
)B

�B
	�B
	B
KB
�B
�B
�B
�B
�B
�B
�B	��B	�"B	�B	��B	��B
 OB
 B
  B	�cB	��B	��B
B	��B	�VB	�jB	��B	��B	��B	��B	�>B	�*B	��B	��B	��B	��B	��B	�0B	��B	��B	��B	��B	�JB	�$B	��B	��B	�+B	��B	��B	�ZB	��B	��B	��B	�B	�B	�B	�B	�aB	�B	�TB	�B	�2B	��B	��B	��B	�2B	��B	�RB	�B	��B	��B	�	B	��B	�B	��B	�2B	�LB	�%B	��B	��B	�B	�B	�FB	�8B	�XB	��B	��B	�rB	�$B	��B	�B	�B	��B	��B	��B	��B	�xB	�xB	�^B	��B	�jB	��B	��B	�B	�dB	�B	��B	��B	��B	��B	��B	��B	�.B	�}B	�B	�HB	��B
 B
 4B
 B	��B
  B
 �B
B
 B
�B
UB
 �B
'B
�B
B
�B
AB
�B
�B
�B
�B
�B
{B
�B
B
�B
�B
[B
uB
B
�B
�B
�B
?B
tB
YB
�B
zB
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
	�B
xB
~B
PB
jB
�B
�B
�B
�B
B
�B
�B
�B
(B
�B
�B
�B
�B
HB
HB
bB
}B
bB
HB
�B
�B
�B
�B
�B
�B
 B
�B
TB
:B
B
�B
�B
�B
uB
[B
@B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
MB
gB
gB
MB
�B
SB

B
�B
�B
B
_B
yB
KB
eB
KB
�B
�B
�B
�B
kB
7B
QB
�B
�B
�B
�B
	B
WB
qB
�B
�B
)B
]B
xB
B
IB
~B
~B
�B
�B
B
�B
�B
!B
�B
VB
VB
�B
 B
 vB
 �B
!-B
!-B
!HB
!|B
!�B
"B
"NB
"�B
"�B
"�B
"�B
#B
#B
"�B
"�B
# B
#nB
#�B
$B
$B
$@B
$�B
%�B
&B
&fB
&�B
&�B
'8B
'�B
(
B
($B
(
B
(>B
(XB
(�B
(�B
(�B
)B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
*B
*B
)�B
)�B
*�B
+�B
,"B
,=B
,"B
,WB
,=B
,=B
,�B
-B
-�B
-�B
-wB
.IB
.�B
/5B
/ B
/�B
/�B
/�B
0UB
0!B
0UB
0;B
0;B
0�B
0!B
/�B
.�B
.�B
.cB
.cB
.�B
.�B
.�B
.�B
.�B
.�B
/OB
/5B
/5B
/5B
/iB
/�B
0�B
0�B
0�B
0�B
0�B
1B
1AB
1�B
2aB
2�B
2�B
2�B
2�B
3MB
3�B
3�B
4B
4B
4TB
4�B
4�B
4�B
4�B
4�B
4�B
4�B
4�B
7B
8�B
9	B
9$B
9XB
9XB
9XB
9rB
9�B
:*B
:*B
9�B
:B
:^B
:DB
:*B
:xB
:�B
:�B
:�B
:�B
;0B
;dB
;�B
<�B
<�B
="B
=<B
=VB
=�B
=�B
>(B
>BB
>(B
>B
>B
>]B
>]B
>B
>]B
?�B
@ B
@iB
A B
AUB
AUB
AoB
AoB
AUB
BB
A�B
A�B
A�B
BAB
BuB
B�B
B�B
CB
C�B
DMB
DgB
D�B
D�B
EB
ESB
ESB
E9B
ESB
EmB
ESB
EmB
E�B
E�B
E�B
FYB
F�B
F�B
F�B
F�B
GB
G+B
GB
F�B
F�B
F�B
F�B
F�B
F�B
G_B
G�B
HKB
H1B
HB
HfB
H�B
H�B
I7B
IRB
I�B
I�B
JrB
J�B
KB
J�B
J�B
J�B
J�B
K)B
KDB
K^B
K�B
K�B
L�B
M�B
M�B
M6B
L�B
K�B
LB
LB
K�B
K�B
LB
L0B
LdB
LJB
LdB
LdB
L�B
MB
MB
MB
N"B
NpB
NpB
O(B
O�B
P}B
P�B
Q B
QB
QhB
RoB
RoB
R�B
R�B
R�B
R�B
R�B
R�B
S&B
S�B
TaB
UB
U�B
U�B
V9B
V�B
VmB
V�B
V�B
V�B
V�B
V�B
W$B
W$B
W?B
W?B
W?B
WYB
WsB
W�B
W�B
WYB
XB
XB
X�B
X�B
X�B
YeB
YB
ZB
Z7B
Z7B
ZQB
ZkB
ZkB
Z�B
Z�B
Z�B
Z�B
Z�B
ZQB
Z7B
Z7B
Z�B
[#B
[WB
[�B
]�B
]�B
]�B
]�B
^B
]�B
]�B
]�B
]�B
]�B
]�B
]�B
]�B
]�B
]�B
]�B
]�B
]�B
]�B
]~B
]�B
]�B
]�B
]�B
]�B
^�B
^�B
^jB
^�B
^�B
^�B
_;B
_;B
_�B
_�B
`BB
`BB
`\B
`�B
`�B
`�B
abB
a|B
a�B
a�B
a�B
a�B
a�B
a�B
a�B
a�B
a�B
bhB
c:B
c B
c:B
c�B
c�B
c�B
c�B
c�B
dZB
e`B
ezB
e�B
e�B
e�B
e�B
gB
g�B
g�B
hXB
h�B
h�B
h�B
h�B
iDB
i_B
i�B
i�B
j0B
jeB
j�B
j�B
j�B
kB
kQB
kQB
k�B
lWB
l�B
m)B
m�B
m�B
m�B
m�B
ncB
nIB
n�B
oiB
pB
p�B
p�B
p�B
qB
qB
qB
qB
q'B
q'B
q'B
qAB
qAB
q[B
qvB
qvB
q�B
q�B
q�B
q�B
r-B
r-B
rB
rB
rB
rB
q�B
q�B
q�B
r�B
r�B
sB
r�B
s3B
s�B
s�B
s�B
s�B
tB
tB
tB
tB
tB
tB
tB
tB
t9B
t9B
tTB
tnB
t�B
t�B
t�B
t�B
t�B
t�B
utB
u�B
u�B
u�B
u�B
u�B
u�B
u�B
vB
vFB
v`B
vFB
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
wLB
w2B
w�B
w�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220609184719  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220609184901  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220609184901  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220609184901                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220610034907  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220610034907  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220609185840                      G�O�G�O�G�O�                