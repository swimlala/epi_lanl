CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:15:10Z creation;2022-06-04T19:15:11Z conversion to V3.1      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604191510  20220610151508  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @��>�ax:1   @��?e�n@0>��"���dƧ1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @@  @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  BffB   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�33B���B�33B�  B�  B�  B�  B�  B�  B�33B���B�  B�ffB�  B�  B�  B�  B�  B�  B���B�  B�  B�33B�  B���B���B�  B�  B���B�  B���C�fC  C  C  C
  C  C  C  C  C  C  C  C  C33C  C�fC!�fC$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CNffCO�fCQ�fCT  CV  CX  CZ  C\  C^  C`  Ca�fCd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C��C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@1�@q�@���@���Az�A<z�A\z�A|z�A�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qB�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B��\B�B�(�B�B��\B��\B��\B��\B��\B��\B�B�(�B��\B���B��\B��\BÏ\BǏ\Bˏ\BϏ\B�\)B׏\Bۏ\B�B�\B�\)B�\)B�\B�\B�(�B��\B�\)C�CǮCǮCǮC	ǮCǮCǮCǮCǮCǮCǮCǮCǮC��CǮC�C!�C#ǮC%ǮC'ǮC)ǮC+ǮC-ǮC/ǮC1ǮC3ǮC5ǮC7ǮC9ǮC;ǮC=ǮC?ǮCAǮCCǮCEǮCGǮCIǮCKǮCN.CO�CQ�CSǮCUǮCWǮCYǮC[ǮC]ǮC_ǮCa�CcǮCeǮCgǮCiǮCkǮCmǮCoǮCqǮCsǮCuǮCwǮCyǮC{ǮC}ǮCǮC��C���C���C���C���C���C��C���C���C���C���C���C���C���C���C��
C��
C���C��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D q�D ��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��D	q�D	��D
q�D
��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��D q�D ��D!q�D!��D"q�D"��D#q�D#��D$q�D$��D%q�D%��D&q�D&��D'q�D'��D(q�D(��D)q�D)��D*q�D*��D+q�D+��D,q�D,��D-q�D-��D.q�D.��D/q�D/��D0q�D0��D1q�D1��D2q�D2��D3q�D3��D4q�D4��D5q�D5��D6q�D6��D7q�D7��D8q�D8��D9q�D9��D:q�D:��D;q�D;��D<q�D<��D=q�D=��D>q�D>��D?q�D?��D@q�D@��DAq�DA��DBq�DB��DCq�DC��DDq�DD��DEq�DE��DFq�DF��DGq�DG��DHq�DH��DIq�DI��DJq�DJ��DKq�DK��DLq�DL��DMq�DM��DNq�DN��DOq�DO��DPq�DP��DQq�DQ��DRq�DR��DSq�DS��DTq�DT��DUq�DU��DVq�DV��DWq�DW��DXq�DX��DYq�DY��DZq�DZ��D[q�D[��D\q�D\��D]q�D]��D^q�D^��D_q�D_��D`q�D`��Daq�Da��Dbq�Db��Dcq�Dc��Ddq�Dd��Deq�De��Dfq�Df��Dgq�Dg��Dhq�Dh��Diq�Di��Djq�Dj��Dkq�Dk��Dlq�Dl��Dmq�Dm��Dnq�Dn��Doq�Do��Dpq�Dp��Dqq�Dq��Drq�Dr��Dsq�Ds��Dtq�Dt��Duq�Du��Dvq�Dv��Dwq�Dw��Dxq�Dx��Dyq�Dy��Dzq�Dz��D{q�D{��D|q�D|��D}q�D}��D~q�D~��Dq�D��D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D��)D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D��)D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D¸�D���D�8�D�x�Dø�D���D�8�D�x�Dĸ�D���D�8�D�x�DŸ�D���D�8�D�x�DƸ�D���D�8�D�x�DǸ�D���D�8�D�x�Dȸ�D���D�8�D�x�Dɸ�D���D�8�D�x�Dʸ�D���D�8�D�x�D˸�D���D�8�D�x�D̸�D���D�8�D�x�D͸�D���D�8�D�x�Dθ�D���D�8�D�x�Dϸ�D���D�8�D�x�Dи�D���D�8�D�x�DѸ�D���D�8�D�x�DҸ�D���D�8�D�x�DӸ�D���D�8�D�x�DԸ�D���D�8�D�x�Dո�D���D�8�D�x�Dָ�D���D�8�D�x�D׸�D���D�8�D�x�Dظ�D���D�8�D�x�Dٸ�D���D�8�D�x�Dڸ�D���D�8�D�x�D۸�D���D�8�D�x�Dܸ�D���D�8�D�x�Dݸ�D���D�8�D�x�D޸�D���D�8�D�x�D߸�D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D���D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�|)D��D���D�8�D�x�D��D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D��)111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A���A��A��fA���A��A��A�3A�wA��A�$A�A䂪A�s�A�tA�tA�t�A�u�A�w�A�xA�wfA�v`A�tA�v`A�v+A�s�A�w2A�u�A��A�AA���A�K�A�S&A��4A�8RA�VAܴ9Aخ�Aץ�Aպ�A�רA�7LA�f�A�F?A�� AʛqAǿ�A�?�A�5�A�:�A��qA�49A���A�u�A�ffA�QA�m]A�xA�Q�A���A��<A�!�A�Z�A���A��qA���A��A���A�c�A�f2A�X�A�N<A��jA�)�A��@A��A~��A{�\Aw�uAqK�AlA�AiN<Ag|�Af�AbC�A^_�A]W?A[�AZ�^AYl"AX�AW7AQ'RAMV�AI�cAG��AFr�ADA@�8A>�!A=�A<N<A:�LA8s�A5��A3��A1�.A1&A0��A0�.A1OvA/~(A,6A+{A(}�A%�uA$bA#  A!�_A �A�cA�wA��A 6A �&A!&�A!:*A ^�A4�A��A4A��A�A��AƨA��A��Ao A�A��A�A��AѷA��A��A�A��A"�A�Au�A5?A�3A�1AI�AA��AFtA�sA�_A��AC�A��A�UA��A�2At�A��Az�AA�A;Ae�AɆA^5A
��A
zxA
&�A	�A	6A�>AB[A��AtTA�AA��AL0AL0AA��AߤA�A_�A=qA�A�A��A^5A�jAv�AVA+A��A?�AOA��A7�A�A �MA ��A �fA ҉A M@�}�@�l"@��@��$@���@�W�@���@��@��@�F�@��e@���@��h@��@�H�@�W�@���@���@�M@���@�c@�ں@�?�@例@��@���@��@�&�@��@�6z@�z@�-@�'R@��@��@��@�}V@��9@�F�@�B�@�<6@��@�\�@彥@䩓@�3�@�6z@��[@�i�@�P�@���@��@���@�Ov@�#:@߂�@޾@�_�@�($@�}�@�@ܾ@�H�@� �@��@�w2@��E@�_�@��@ٸ�@�6z@�oi@�g�@�!-@�%F@��@���@֘_@�Ta@�/�@���@�RT@ԍ�@��>@Ӈ�@��@ҸR@��@�W?@��)@��@�k�@�S&@�1�@�ȴ@�_@�1@�a@̹$@�c @�	@�qv@��@���@�Ft@ɵt@�Vm@���@�@Ƿ�@�[W@��/@Ƴh@Ƈ+@�tT@�_@�Z�@�S�@�#:@�+@�ff@�+k@�>�@�oi@��.@���@���@���@��@�W�@��@�X�@�C@��@��@��@��@�j@��@�u%@��)@��@��n@���@�5?@���@���@�e�@��@�_�@��g@�`B@�q@��@��r@�N�@�L0@��@��T@�dZ@��@��\@�|�@��@�k�@���@��D@���@��@@��@���@���@��b@�w�@��@��h@��@�Z@�0U@�O@��@���@��T@���@��d@���@���@��h@��f@���@��{@�dZ@��@���@�q@�ff@�-@�� @�x�@�c @��&@���@�8@���@���@�Q@��@���@�U�@�ߤ@�	@��@���@���@��@���@�~�@�m]@��@���@���@��"@���@�:*@��.@��"@��@��'@�v�@��@���@�dZ@�K�@�=@��@��5@�Xy@��@���@���@�,�@��1@�:*@��Z@��@��'@���@�5�@�"�@��@��K@��O@�>B@��@���@�[W@��@�[�@��@��:@�E9@��@�ѷ@�H�@�_@��g@��:@�6z@��P@���@��@�x�@�N<@�;@��e@�Q@��@���@���@�f�@�@��@�kQ@��@��@��T@���@���@�f�@�o@�֡@���@�~(@�-@�˒@��C@��~@�qv@�0�@�Ɇ@��@�?�@�@��)@���@��V@�U�@�+�@��v@�w�@�,=@���@��*@�g�@�J#@�+@� i@��E@�ȴ@��F@��o@��S@�zx@�L�@�q@��K@��U@���@�Ta@�@��W@��6@��n@���@�s@�n/@�dZ@�@O@�0�@�C@��@��M@���@��_@�Ft@��N@�0�@���@��u@�n�@�-�@��@���@��@@�s�@�T�@�#�@��@�ߤ@���@�z�@�A�@�	@P�@!-@C@C@~�'@~v�@~8�@}��@}-w@|ѷ@|�?@|��@|[�@{�k@z�X@z�6@z�!@z��@z�'@z��@z�@z��@zJ@y�@x��@x��@x]d@xXy@x~@w�r@w��@wO@wC@v�@v@�@u�@uS&@u�@t9X@s��@s� @s�0@s��@r��@r:*@q�o@q*0@pZ@o�@o��@oZ�@n�L@m��@m��@m\�@l�)@l  @kiD@j�"@j�@i��@iL�@i(�@h�I@hZ@h/�@h�@g��@g��@g$t@g�@f҉@f��@f8�@e�d@e�@e`B@d��@d��@d7�@c�]@c�;@c��@c$t@b��@b3�@a��@aJ�@`�@`�@`?�@`��@`��@`�@`u�@`S�@`�@_��@_��@_Z�@^��@^�@^{�@^�@]�C@]��@]a�@]J�@]7L@\�`@\��@\�@[��@Z�\@ZE�@Z@YB�@X��@X�@X�@X!@W��@V�"@U��@U/@T�@T�e@TS�@T!@S�@S��@S�k@SO@R��@Q��@Q��@Q@P�@P�@O�*@Ob�@O+@N�@NL0@M8�@L��@Lj@K��@K=@J�H@J�@JV@I�z@IL�@H��@H[�@G��@GRT@G$t@G�@F��@F�h@F��@FOv@E�H@EF@E�@D��@C�A@C��@Cqv@CA�@C4�@C$t@B��@Bff@B{@B_@B	@A�Z@A�N@A�@A��@AG�@A+@A	l@@�P@@��@@6@@�@?� @?v`@?6z@>�B@>�r@>a|@>3�@=��@=��@=��@=s�@=N<@=5�@=�@<�@<�4@<w�@<I�@<"h@;�;@;�P@;/�@:��@:�h@:�1@:�A@:1�@9�@9�C@9��@9m]@9J�@9(�@8�`@8��@8!@7��@7�w@7��@7��@7g�@7K�@7;d@7�@7�@6�H@6��@6;�@6u@5�@5��@5�H@5��@5s�@5/@4�.@4_@4�@3�W@3خ@3��@3x@3e�@31�@3�@2҉@2�}@2��@2=q@1�@1�=@1k�@1;@0�[@0�4@0Z@0b@/�@/��@/�F@/�$@/iD@/'�@.�@.��@.8�@.{@-�>@-��@-rG@-�@,�z@,@+��@+P�@+E9@+F�@+.I@*�X@*\�@)��@)�n@)?}@(Ɇ@(<�@'�}@'�f@'qv@'dZ@'Mj@'9�@&�M@&��@&��@&�\@&\�@&.�@%�3@%rG@$�/@$$@#��@#H�@#)_@"�H@"��@"u%@"!�@"@!�@!�@!�7@!8�@!/@!%F@!�@ �`@ �o@ �@��@�0@��@��@��@��@e�@_p@P�@Y@ں@^5@:*@��@�'@��@Q�@��@��@m�@N�@H@�@�}@�@�	@�@�@�@�<@��@Q@@�@T�@��@�_@��@~@�K@{J@e�@X�@J#@�@��@�L@E�@�@�@��@�@��@T�@��@��@Xy@��@v`@=@o@z@?@�@��@u�@4@�@��@�@��@N�@:�@	�@��@��@C�@1�@�@�@��@�6@�r@h
@;�@O@J@�Z@�9@��@�~@4@�@;@�5@��@:�@  @��@��@�;@ƨ@�k@�f@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A���A��A��fA���A��A��A�3A�wA��A�$A�A䂪A�s�A�tA�tA�t�A�u�A�w�A�xA�wfA�v`A�tA�v`A�v+A�s�A�w2A�u�A��A�AA���A�K�A�S&A��4A�8RA�VAܴ9Aخ�Aץ�Aպ�A�רA�7LA�f�A�F?A�� AʛqAǿ�A�?�A�5�A�:�A��qA�49A���A�u�A�ffA�QA�m]A�xA�Q�A���A��<A�!�A�Z�A���A��qA���A��A���A�c�A�f2A�X�A�N<A��jA�)�A��@A��A~��A{�\Aw�uAqK�AlA�AiN<Ag|�Af�AbC�A^_�A]W?A[�AZ�^AYl"AX�AW7AQ'RAMV�AI�cAG��AFr�ADA@�8A>�!A=�A<N<A:�LA8s�A5��A3��A1�.A1&A0��A0�.A1OvA/~(A,6A+{A(}�A%�uA$bA#  A!�_A �A�cA�wA��A 6A �&A!&�A!:*A ^�A4�A��A4A��A�A��AƨA��A��Ao A�A��A�A��AѷA��A��A�A��A"�A�Au�A5?A�3A�1AI�AA��AFtA�sA�_A��AC�A��A�UA��A�2At�A��Az�AA�A;Ae�AɆA^5A
��A
zxA
&�A	�A	6A�>AB[A��AtTA�AA��AL0AL0AA��AߤA�A_�A=qA�A�A��A^5A�jAv�AVA+A��A?�AOA��A7�A�A �MA ��A �fA ҉A M@�}�@�l"@��@��$@���@�W�@���@��@��@�F�@��e@���@��h@��@�H�@�W�@���@���@�M@���@�c@�ں@�?�@例@��@���@��@�&�@��@�6z@�z@�-@�'R@��@��@��@�}V@��9@�F�@�B�@�<6@��@�\�@彥@䩓@�3�@�6z@��[@�i�@�P�@���@��@���@�Ov@�#:@߂�@޾@�_�@�($@�}�@�@ܾ@�H�@� �@��@�w2@��E@�_�@��@ٸ�@�6z@�oi@�g�@�!-@�%F@��@���@֘_@�Ta@�/�@���@�RT@ԍ�@��>@Ӈ�@��@ҸR@��@�W?@��)@��@�k�@�S&@�1�@�ȴ@�_@�1@�a@̹$@�c @�	@�qv@��@���@�Ft@ɵt@�Vm@���@�@Ƿ�@�[W@��/@Ƴh@Ƈ+@�tT@�_@�Z�@�S�@�#:@�+@�ff@�+k@�>�@�oi@��.@���@���@���@��@�W�@��@�X�@�C@��@��@��@��@�j@��@�u%@��)@��@��n@���@�5?@���@���@�e�@��@�_�@��g@�`B@�q@��@��r@�N�@�L0@��@��T@�dZ@��@��\@�|�@��@�k�@���@��D@���@��@@��@���@���@��b@�w�@��@��h@��@�Z@�0U@�O@��@���@��T@���@��d@���@���@��h@��f@���@��{@�dZ@��@���@�q@�ff@�-@�� @�x�@�c @��&@���@�8@���@���@�Q@��@���@�U�@�ߤ@�	@��@���@���@��@���@�~�@�m]@��@���@���@��"@���@�:*@��.@��"@��@��'@�v�@��@���@�dZ@�K�@�=@��@��5@�Xy@��@���@���@�,�@��1@�:*@��Z@��@��'@���@�5�@�"�@��@��K@��O@�>B@��@���@�[W@��@�[�@��@��:@�E9@��@�ѷ@�H�@�_@��g@��:@�6z@��P@���@��@�x�@�N<@�;@��e@�Q@��@���@���@�f�@�@��@�kQ@��@��@��T@���@���@�f�@�o@�֡@���@�~(@�-@�˒@��C@��~@�qv@�0�@�Ɇ@��@�?�@�@��)@���@��V@�U�@�+�@��v@�w�@�,=@���@��*@�g�@�J#@�+@� i@��E@�ȴ@��F@��o@��S@�zx@�L�@�q@��K@��U@���@�Ta@�@��W@��6@��n@���@�s@�n/@�dZ@�@O@�0�@�C@��@��M@���@��_@�Ft@��N@�0�@���@��u@�n�@�-�@��@���@��@@�s�@�T�@�#�@��@�ߤ@���@�z�@�A�@�	@P�@!-@C@C@~�'@~v�@~8�@}��@}-w@|ѷ@|�?@|��@|[�@{�k@z�X@z�6@z�!@z��@z�'@z��@z�@z��@zJ@y�@x��@x��@x]d@xXy@x~@w�r@w��@wO@wC@v�@v@�@u�@uS&@u�@t9X@s��@s� @s�0@s��@r��@r:*@q�o@q*0@pZ@o�@o��@oZ�@n�L@m��@m��@m\�@l�)@l  @kiD@j�"@j�@i��@iL�@i(�@h�I@hZ@h/�@h�@g��@g��@g$t@g�@f҉@f��@f8�@e�d@e�@e`B@d��@d��@d7�@c�]@c�;@c��@c$t@b��@b3�@a��@aJ�@`�@`�@`?�@`��@`��@`�@`u�@`S�@`�@_��@_��@_Z�@^��@^�@^{�@^�@]�C@]��@]a�@]J�@]7L@\�`@\��@\�@[��@Z�\@ZE�@Z@YB�@X��@X�@X�@X!@W��@V�"@U��@U/@T�@T�e@TS�@T!@S�@S��@S�k@SO@R��@Q��@Q��@Q@P�@P�@O�*@Ob�@O+@N�@NL0@M8�@L��@Lj@K��@K=@J�H@J�@JV@I�z@IL�@H��@H[�@G��@GRT@G$t@G�@F��@F�h@F��@FOv@E�H@EF@E�@D��@C�A@C��@Cqv@CA�@C4�@C$t@B��@Bff@B{@B_@B	@A�Z@A�N@A�@A��@AG�@A+@A	l@@�P@@��@@6@@�@?� @?v`@?6z@>�B@>�r@>a|@>3�@=��@=��@=��@=s�@=N<@=5�@=�@<�@<�4@<w�@<I�@<"h@;�;@;�P@;/�@:��@:�h@:�1@:�A@:1�@9�@9�C@9��@9m]@9J�@9(�@8�`@8��@8!@7��@7�w@7��@7��@7g�@7K�@7;d@7�@7�@6�H@6��@6;�@6u@5�@5��@5�H@5��@5s�@5/@4�.@4_@4�@3�W@3خ@3��@3x@3e�@31�@3�@2҉@2�}@2��@2=q@1�@1�=@1k�@1;@0�[@0�4@0Z@0b@/�@/��@/�F@/�$@/iD@/'�@.�@.��@.8�@.{@-�>@-��@-rG@-�@,�z@,@+��@+P�@+E9@+F�@+.I@*�X@*\�@)��@)�n@)?}@(Ɇ@(<�@'�}@'�f@'qv@'dZ@'Mj@'9�@&�M@&��@&��@&�\@&\�@&.�@%�3@%rG@$�/@$$@#��@#H�@#)_@"�H@"��@"u%@"!�@"@!�@!�@!�7@!8�@!/@!%F@!�@ �`@ �o@ �@��@�0@��@��@��@��@e�@_p@P�@Y@ں@^5@:*@��@�'@��@Q�@��@��@m�@N�@H@�@�}@�@�	@�@�@�@�<@��@Q@@�@T�@��@�_@��@~@�K@{J@e�@X�@J#@�@��@�L@E�@�@�@��@�@��@T�@��@��@Xy@��@v`@=@o@z@?@�@��@u�@4@�@��@�@��@N�@:�@	�@��@��@C�@1�@�@�@��@�6@�r@h
@;�@O@J@�Z@�9@��@�~@4@�@;@�5@��@:�@  @��@��@�;@ƨ@�k@�f@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�2B��B��B��B��B�B��B�,B�B�FB��B�,B��B�B�B�`B��B�B�8B�8B�B�>B��B��B�QB��B�B�	B	+B	'�B	]�B	��B	�[B	��B	�B	��B	��B	�hB	��B	��B	�:B	��B	��B	��B	�-B	�cB	��B	��B	�_B	ݘB	��B	��B	�B
/B
�B	��B
.B
�B
�yB
��B
��B
�LB
�B
��B
�`B
��B
�B
��B
m�B
O�B
# B
3B	�OB	ңB	��B	�oB	�nB	�&B	�B	k�B	[WB	h�B	i_B	hXB	W�B	@�B	;�B	6FB	1�B	,WB	#�B	xB	�B��B�}B�]B��B�KB�LB�B��B��B�B�7B�9B�NB�B�B� B	IB	>wB	K^B	2|B	(
B	�B	�B��B�B�
B�-B�B��B�B�B	�B	B	-)B	E�B	A B	@�B	AB	?�B	@ B	VB	iB	g8B	g�B	j�B	]�B	V9B	Y�B	bNB	d�B	t�B	y�B	��B	��B	��B	�(B	�}B	�oB	��B	��B	�KB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�IB	��B	�B	��B	�3B	�|B	�MB	�tB	�+B	�FB	��B	��B	�LB	��B	��B	�fB	��B	�?B	�B	�8B	��B	�rB	�dB	��B	��B	�(B	��B	��B	��B	�oB	��B	��B	��B	��B	�uB	āB	ĜB	āB	�3B	ŢB	żB	ňB	�B	ĶB	ðB	��B	ňB	�9B	ƎB	��B	��B	�rB	��B	�~B	��B	�PB	�"B	�B	�<B	�VB	̈́B	�jB	�pB	�VB	�VB	�bB	ҽB	ңB	�@B	�,B	�B	�
B	�B	ӏB	� B	�B	� B	� B	��B	�4B	��B	��B	��B	ңB	ՁB	�B	��B	ՁB	��B	ӏB	�B	�aB	�gB	ٚB	�1B	��B	ؓB	ٚB	�1B	�B	�B	�QB	�B	��B	�B	�kB	��B	یB	�qB	��B	��B	߾B	�!B	ߤB	��B	�jB	ݲB	ބB	�OB	��B	��B	�B	��B	�&B	�nB	�B	��B	�B	��B	��B	�B	�B	� B	�B	�B	�nB	�nB	�nB	��B	�ZB	�B	��B	��B	�LB	�B	��B	�B	�B	�RB	�B	�
B	�XB	�_B	�eB	��B	��B	�B	��B	��B	��B	��B	�B	�*B	�*B	�B	�B	�0B	��B	�6B	��B	�)B	��B	�]B	��B	�cB	�}B	��B	�;B	��B	�UB	�B	��B	�B	�9B	��B	��B	��B	�2B	��B	��B	��B	�>B	�	B	�>B	��B	�fB	��B	�FB	��B	�%B	�B	��B	�9B	�B	�|B	��B	�aB	��B	�B	��B	�aB	��B	��B	�B	�B	�tB	�ZB	�tB	��B	��B	��B	��B	�RB	�RB	�RB	�8B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�XB	��B	��B	�XB	�$B	��B	�	B	��B	�0B	��B	��B	�VB	��B	�wB	��B	�.B	��B
 OB
�B
'B
AB
AB
[B
[B
AB
AB
-B
�B
�B
�B
B
�B
�B
�B
�B
B
B
�B
?B
?B
YB
YB
YB
YB
B
zB
EB
�B
�B
�B
	RB
	�B

�B

�B
�B
JB
dB
0B
�B
�B
~B
�B
B
jB
jB
<B
�B
(B
�B
�B
HB
 B
NB
�B
�B
:B
:B
�B
uB
{B
FB
aB
,B
�B
2B
�B
�B
�B
B
�B
$B
�B
�B
�B
�B
+B
_B
B
B
kB
�B
�B
=B
qB
�B
�B
�B
B
�B
�B
B
jB
�B
�B
VB
VB
pB
VB
VB
�B
�B
�B
�B
�B
 'B
 \B
 'B
 �B
 B
�B
�B
�B
pB
VB
;B
!B
�B
B
�B
�B
�B
!B
pB
�B
�B
 vB
 �B
!|B
!�B
!�B
!�B
!�B
"B
"4B
 �B
 \B
 B
�B
 \B
!B
 �B
!B
!B
!�B
!|B
!bB
!bB
!|B
"hB
"�B
"�B
#�B
#�B
$@B
$�B
$�B
$�B
$tB
%�B
&LB
&fB
'8B
(>B
'�B
'�B
(�B
)B
)DB
)yB
)�B
)�B
*�B
-�B
.cB
-�B
-�B
-�B
-�B
.�B
0!B
2�B
4TB
4nB
4�B
4�B
4�B
4�B
4�B
5�B
5tB
5ZB
5tB
5tB
5tB
6FB
6zB
6�B
6FB
6�B
7fB
7�B
8B
8�B
8�B
8�B
8�B
9	B
9$B
8lB
8B
7�B
8B
8B
8B
8lB
8RB
88B
88B
8�B
9	B
9	B
9	B
9$B
9XB
9�B
:*B
:*B
:^B
:�B
:�B
:�B
;B
;�B
<6B
;�B
;�B
<jB
=VB
>(B
=�B
=�B
>BB
?�B
@�B
A�B
A�B
BB
B[B
B�B
B�B
CGB
C�B
C�B
C�B
C�B
DMB
D�B
D�B
D�B
D�B
D�B
E9B
EmB
EmB
E9B
E9B
E�B
F?B
E�B
F�B
GEB
G�B
H�B
H1B
HKB
HKB
H�B
IB
I7B
I�B
I�B
I�B
J=B
J�B
J�B
J#B
JXB
JrB
J�B
KxB
K�B
K�B
LB
LB
L�B
M�B
NB
NVB
N�B
OBB
O�B
O�B
O�B
PbB
P�B
P�B
P�B
QNB
Q�B
Q�B
Q�B
Q�B
RB
Q�B
RB
RoB
R�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
TFB
T�B
T{B
T{B
T�B
T�B
T�B
T�B
T�B
UB
UB
T�B
U2B
U�B
U�B
U�B
U�B
VB
VSB
V�B
V�B
V�B
V�B
V�B
V�B
W
B
W?B
W?B
W?B
WYB
WsB
W�B
W�B
W�B
W�B
X+B
X_B
X�B
X�B
X�B
X�B
X�B
Y1B
YeB
YeB
Y�B
Y�B
Y�B
Y�B
ZB
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
[#B
[#B
[=B
[=B
[WB
[�B
[�B
\B
\)B
\)B
\CB
\CB
\]B
\�B
]B
]/B
]dB
]~B
]~B
]�B
]�B
]�B
]�B
^B
^5B
^OB
^OB
^�B
^�B
^�B
_!B
_pB
_�B
_�B
_�B
`B
`BB
`BB
`vB
`\B
`�B
`�B
`�B
aB
aHB
aHB
a|B
a|B
a�B
a�B
a�B
b�B
c B
c B
c B
cB
c B
c�B
c�B
d@B
dZB
d�B
d�B
ezB
e�B
f2B
fLB
fLB
ffB
fLB
f�B
f�B
f�B
f�B
gB
gB
gmB
g�B
h$B
hsB
h�B
i_B
iDB
i�B
i�B
i�B
jKB
jKB
jKB
jeB
j�B
kB
kB
kB
kB
kB
k�B
k�B
l"B
l"B
l"B
l"B
l"B
l=B
lWB
lWB
lWB
lqB
l�B
l�B
l�B
mB
m]B
m]B
m�B
nB
n�B
n�B
n�B
n�B
n�B
o B
o B
oOB
o�B
o�B
o�B
pB
pB
pUB
p�B
p�B
q'B
q�B
q�B
q�B
rB
raB
r�B
r�B
r�B
r�B
sB
sB
s3B
s�B
tB
t9B
t9B
tTB
t9B
tnB
t�B
t�B
t�B
u�B
v+B
vFB
vFB
wB
w2B
w�B
w�B
w�B
x8B
x8B
xRB
xRB
x�B
x�B
y	B
y$B
y>B
y�B
y�B
y�B
zB
z*B
zDB
z^B
zxB
z�B
z�B
z�B
z�B
z�B
{B
{0B
{0B
{�B
{�B
{�B
{�B
|B
|�B
|�B
|�B
|�B
|�B
}B
}VB
}<B
}<111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�2B��B��B��B��B�B��B�,B�B�FB��B�,B��B�B�B�`B��B�B�8B�8B�B�>B��B��B�QB��B�B�	B	+B	'�B	]�B	��B	�[B	��B	�B	��B	��B	�hB	��B	��B	�:B	��B	��B	��B	�-B	�cB	��B	��B	�_B	ݘB	��B	��B	�B
/B
�B	��B
.B
�B
�yB
��B
��B
�LB
�B
��B
�`B
��B
�B
��B
m�B
O�B
# B
3B	�OB	ңB	��B	�oB	�nB	�&B	�B	k�B	[WB	h�B	i_B	hXB	W�B	@�B	;�B	6FB	1�B	,WB	#�B	xB	�B��B�}B�]B��B�KB�LB�B��B��B�B�7B�9B�NB�B�B� B	IB	>wB	K^B	2|B	(
B	�B	�B��B�B�
B�-B�B��B�B�B	�B	B	-)B	E�B	A B	@�B	AB	?�B	@ B	VB	iB	g8B	g�B	j�B	]�B	V9B	Y�B	bNB	d�B	t�B	y�B	��B	��B	��B	�(B	�}B	�oB	��B	��B	�KB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�IB	��B	�B	��B	�3B	�|B	�MB	�tB	�+B	�FB	��B	��B	�LB	��B	��B	�fB	��B	�?B	�B	�8B	��B	�rB	�dB	��B	��B	�(B	��B	��B	��B	�oB	��B	��B	��B	��B	�uB	āB	ĜB	āB	�3B	ŢB	żB	ňB	�B	ĶB	ðB	��B	ňB	�9B	ƎB	��B	��B	�rB	��B	�~B	��B	�PB	�"B	�B	�<B	�VB	̈́B	�jB	�pB	�VB	�VB	�bB	ҽB	ңB	�@B	�,B	�B	�
B	�B	ӏB	� B	�B	� B	� B	��B	�4B	��B	��B	��B	ңB	ՁB	�B	��B	ՁB	��B	ӏB	�B	�aB	�gB	ٚB	�1B	��B	ؓB	ٚB	�1B	�B	�B	�QB	�B	��B	�B	�kB	��B	یB	�qB	��B	��B	߾B	�!B	ߤB	��B	�jB	ݲB	ބB	�OB	��B	��B	�B	��B	�&B	�nB	�B	��B	�B	��B	��B	�B	�B	� B	�B	�B	�nB	�nB	�nB	��B	�ZB	�B	��B	��B	�LB	�B	��B	�B	�B	�RB	�B	�
B	�XB	�_B	�eB	��B	��B	�B	��B	��B	��B	��B	�B	�*B	�*B	�B	�B	�0B	��B	�6B	��B	�)B	��B	�]B	��B	�cB	�}B	��B	�;B	��B	�UB	�B	��B	�B	�9B	��B	��B	��B	�2B	��B	��B	��B	�>B	�	B	�>B	��B	�fB	��B	�FB	��B	�%B	�B	��B	�9B	�B	�|B	��B	�aB	��B	�B	��B	�aB	��B	��B	�B	�B	�tB	�ZB	�tB	��B	��B	��B	��B	�RB	�RB	�RB	�8B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�XB	��B	��B	�XB	�$B	��B	�	B	��B	�0B	��B	��B	�VB	��B	�wB	��B	�.B	��B
 OB
�B
'B
AB
AB
[B
[B
AB
AB
-B
�B
�B
�B
B
�B
�B
�B
�B
B
B
�B
?B
?B
YB
YB
YB
YB
B
zB
EB
�B
�B
�B
	RB
	�B

�B

�B
�B
JB
dB
0B
�B
�B
~B
�B
B
jB
jB
<B
�B
(B
�B
�B
HB
 B
NB
�B
�B
:B
:B
�B
uB
{B
FB
aB
,B
�B
2B
�B
�B
�B
B
�B
$B
�B
�B
�B
�B
+B
_B
B
B
kB
�B
�B
=B
qB
�B
�B
�B
B
�B
�B
B
jB
�B
�B
VB
VB
pB
VB
VB
�B
�B
�B
�B
�B
 'B
 \B
 'B
 �B
 B
�B
�B
�B
pB
VB
;B
!B
�B
B
�B
�B
�B
!B
pB
�B
�B
 vB
 �B
!|B
!�B
!�B
!�B
!�B
"B
"4B
 �B
 \B
 B
�B
 \B
!B
 �B
!B
!B
!�B
!|B
!bB
!bB
!|B
"hB
"�B
"�B
#�B
#�B
$@B
$�B
$�B
$�B
$tB
%�B
&LB
&fB
'8B
(>B
'�B
'�B
(�B
)B
)DB
)yB
)�B
)�B
*�B
-�B
.cB
-�B
-�B
-�B
-�B
.�B
0!B
2�B
4TB
4nB
4�B
4�B
4�B
4�B
4�B
5�B
5tB
5ZB
5tB
5tB
5tB
6FB
6zB
6�B
6FB
6�B
7fB
7�B
8B
8�B
8�B
8�B
8�B
9	B
9$B
8lB
8B
7�B
8B
8B
8B
8lB
8RB
88B
88B
8�B
9	B
9	B
9	B
9$B
9XB
9�B
:*B
:*B
:^B
:�B
:�B
:�B
;B
;�B
<6B
;�B
;�B
<jB
=VB
>(B
=�B
=�B
>BB
?�B
@�B
A�B
A�B
BB
B[B
B�B
B�B
CGB
C�B
C�B
C�B
C�B
DMB
D�B
D�B
D�B
D�B
D�B
E9B
EmB
EmB
E9B
E9B
E�B
F?B
E�B
F�B
GEB
G�B
H�B
H1B
HKB
HKB
H�B
IB
I7B
I�B
I�B
I�B
J=B
J�B
J�B
J#B
JXB
JrB
J�B
KxB
K�B
K�B
LB
LB
L�B
M�B
NB
NVB
N�B
OBB
O�B
O�B
O�B
PbB
P�B
P�B
P�B
QNB
Q�B
Q�B
Q�B
Q�B
RB
Q�B
RB
RoB
R�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
TFB
T�B
T{B
T{B
T�B
T�B
T�B
T�B
T�B
UB
UB
T�B
U2B
U�B
U�B
U�B
U�B
VB
VSB
V�B
V�B
V�B
V�B
V�B
V�B
W
B
W?B
W?B
W?B
WYB
WsB
W�B
W�B
W�B
W�B
X+B
X_B
X�B
X�B
X�B
X�B
X�B
Y1B
YeB
YeB
Y�B
Y�B
Y�B
Y�B
ZB
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
[#B
[#B
[=B
[=B
[WB
[�B
[�B
\B
\)B
\)B
\CB
\CB
\]B
\�B
]B
]/B
]dB
]~B
]~B
]�B
]�B
]�B
]�B
^B
^5B
^OB
^OB
^�B
^�B
^�B
_!B
_pB
_�B
_�B
_�B
`B
`BB
`BB
`vB
`\B
`�B
`�B
`�B
aB
aHB
aHB
a|B
a|B
a�B
a�B
a�B
b�B
c B
c B
c B
cB
c B
c�B
c�B
d@B
dZB
d�B
d�B
ezB
e�B
f2B
fLB
fLB
ffB
fLB
f�B
f�B
f�B
f�B
gB
gB
gmB
g�B
h$B
hsB
h�B
i_B
iDB
i�B
i�B
i�B
jKB
jKB
jKB
jeB
j�B
kB
kB
kB
kB
kB
k�B
k�B
l"B
l"B
l"B
l"B
l"B
l=B
lWB
lWB
lWB
lqB
l�B
l�B
l�B
mB
m]B
m]B
m�B
nB
n�B
n�B
n�B
n�B
n�B
o B
o B
oOB
o�B
o�B
o�B
pB
pB
pUB
p�B
p�B
q'B
q�B
q�B
q�B
rB
raB
r�B
r�B
r�B
r�B
sB
sB
s3B
s�B
tB
t9B
t9B
tTB
t9B
tnB
t�B
t�B
t�B
u�B
v+B
vFB
vFB
wB
w2B
w�B
w�B
w�B
x8B
x8B
xRB
xRB
x�B
x�B
y	B
y$B
y>B
y�B
y�B
y�B
zB
z*B
zDB
z^B
zxB
z�B
z�B
z�B
z�B
z�B
{B
{0B
{0B
{�B
{�B
{�B
{�B
|B
|�B
|�B
|�B
|�B
|�B
}B
}VB
}<B
}<111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105231  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604191510  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604191510  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604191511                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605041519  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605041519  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610151508                      G�O�G�O�G�O�                