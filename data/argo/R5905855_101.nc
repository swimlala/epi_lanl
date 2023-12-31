CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:28:39Z creation;2022-06-04T19:28:40Z conversion to V3.1      
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
_FillValue                 �  I<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M,   PRES_ADJUSTED_QC         
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
_FillValue                 �  �@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
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
resolution        :�o     �  �D   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ޼   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �$   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �(   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �h   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �x   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �|   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604192839  20220610161505  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               eA   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @٣MI�t1   @٣M�V�@+�p��
=�d"�x���1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�33@���@���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B���B�33B�ffB���B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  C   C�fC�fC�fC  C
  C�C�C  C  C  C�fC  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2ffC4  C5�fC8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\�C^L�C`�Ca�fCc�fCe�fCg�fCj  Cl  Cn  Cp  Cr  Ct  Cu�fCw�fCz  C|  C~  C�  C�  C��3C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5fD5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DMfDM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�3D�C3Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D���D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�@xQ�@�@�Az�A<z�A\z�A|z�A�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qB�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B�B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B�\)B��\B��\B�(�B�B���B�\)BϏ\Bӏ\B׏\Bۏ\Bߏ\B�\B�\B�\B�\B�\)B��\B��\B��\C�C�C�CǮC	ǮC�HC�HCǮCǮCǮC�CǮCǮCǮCǮCǮC!ǮC#ǮC%ǮC'ǮC)ǮC+ǮC-ǮC/ǮC2.C3ǮC5�C7ǮC9ǮC;ǮC=ǮC?ǮCAǮCCǮCEǮCGǮCIǮCKǮCMǮCOǮCQǮCSǮCUǮCWǮCYǮC[�HC^{C_�HCa�Cc�Ce�Cg�CiǮCkǮCmǮCoǮCqǮCsǮCu�Cw�CyǮC{ǮC}ǮCǮC���C��
C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D q�D ��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��D	q�D	��D
q�D
��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D�RDq�D��Dq�D��Dq�D��Dq�D��Dq�D��D q�D ��D!q�D!��D"q�D"��D#q�D#��D$q�D$��D%q�D%��D&q�D&��D'q�D'��D(q�D(��D)q�D)��D*q�D*��D+q�D+��D,q�D,��D-q�D-��D.q�D.��D/q�D/��D0q�D0��D1q�D1��D2q�D2��D3q�D3��D4q�D4�RD5q�D5��D6q�D6��D7q�D7��D8q�D8��D9q�D9��D:q�D:��D;q�D;��D<q�D<��D=q�D=��D>q�D>��D?q�D?��D@q�D@��DAq�DA��DBq�DB��DCq�DC��DDq�DD��DEq�DE��DFq�DF��DGq�DG��DHq�DH��DIq�DI��DJq�DJ��DKq�DK��DLq�DL�RDMq�DM��DNq�DN��DOq�DO��DPq�DP��DQq�DQ��DRq�DR��DSq�DS��DTq�DT��DUq�DU��DVq�DV��DWq�DW��DXq�DX��DYq�DY��DZq�DZ��D[q�D[��D\q�D\��D]q�D]��D^q�D^��D_q�D_��D`q�D`��Daq�Da��Dbq�Db��Dcq�Dc��Ddq�Dd��Deq�De��Dfq�Df��Dgq�Dg��Dhq�Dh��Diq�Di��Djq�Dj��Dkq�Dk��Dlq�Dl��Dmq�Dm��Dnq�Dn��Doq�Do��Dpq�Dp��Dqq�Dq��Drq�Dr��Dsq�Ds��Dtq�Dt��Duq�Du��Dvq�Dv��Dwq�Dw��Dxq�Dx��Dyq�Dy��Dzq�Dz��D{q�D{��D|q�D|��D}q�D}��D~q�D~��Dq�D��D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D¸�D���D�8�D�x�Dø�D���D�8�D�x�Dĸ�D���D�8�D�x�DŸ�D���D�8�D�x�DƸ�D��)D�<)D�x�DǸ�D���D�8�D�x�Dȸ�D���D�8�D�x�Dɸ�D���D�8�D�x�Dʸ�D���D�8�D�x�D˸�D���D�8�D�x�D̸�D���D�8�D�x�D͸�D���D�8�D�x�Dθ�D���D�8�D�x�Dϸ�D���D�8�D�x�Dи�D���D�8�D�x�DѸ�D���D�8�D�x�DҸ�D���D�8�D�x�DӸ�D���D�8�D�x�DԸ�D���D�8�D�x�Dո�D���D�8�D�x�Dָ�D���D�8�D�x�D׸�D���D�8�D�x�Dظ�D���D�8�D�x�Dٸ�D���D�8�D�x�Dڸ�D���D�8�D�x�D۸�D���D�8�D�x�Dܸ�D���D�8�D�x�Dݸ�D���D�8�D�x�D޸�D���D�8�D�x�D߸�D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D���D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��vA��A���A���A���A��mA�ǮA�|�A�?�A��AկAչ�Aռ�Aպ�A��)A��A��A�A���A�� A��BA���A���A���A��%A��,A�� A���A���AղaAըXA՜�AՉ�A�u%A�i�A�g�A�a|A�ZA�ZA�XyA�M�A�IA��;A�یAԴ�A���A�w2A�$tA�~�AȍAƹ$A��A��A��OA��A��MA�m]A�'�A�� A�{�A���A��A�x�A��/A�.�A�ZA�t�A�/�A���A�E9A��A��A�(XA��A��A���A��A�{�A��{A��hA���A��A�B'A���A�7A�6�A�;�A��A��qA�"4A�C�A�;Ax�LAv�7At�}ApϫAk;AgVAc(A`�A^S&AZ��AWoiAT@AQ�AM�AL|AIg�AH($AGԕAF�AE��AB<6A:��A5�A24�A/j�A.��A-�CA,�PA,��A+��A)�A'��A'�aA'��A'�A&�AA&J#A%�)A$��A$~A#�KA"�1A!E9A!�A��A��A�A��A��A��A��A��A�A+A��A�zA!�A��A�A9XAzxA�A��A
�AXyA��A�A�RAl�AA�A��AQ�AOA�`A3�A�A��A��A֡A�4A��A�aA��AA'RA��A��AU�A@OA
�&A
*�A
e�A
	�A	�uA	|�A�pAo�A	lA��A�AI�A��A��A#�A �Av`A��Az�A �Ap;A�A ��A ��A ��A ��A �IA �oA rGA�A�TA�FA�AC�A S�@���@��O@��r@�_@��Z@�
�@�{J@��$@��@��`@�kQ@�=@��n@��@��4@�1�@�Q�@�G�@�U2@�}�@�Mj@�p;@�a@@��3@�[W@��@�@�:*@�$@�k�@�\@�ݘ@�C�@���@�s�@�$�@���@�:@�@��@���@��@�]d@�	@�˒@��@��@���@�l"@�-�@߇�@��2@�Ta@��;@��@�l"@۷�@�f�@�;@�,=@٦�@�j@�!�@�u%@ד@ְ!@�)�@�?}@Ԙ_@��@��T@ӊ	@�[W@�Z@�|�@п�@�p;@�Xy@��@� i@Λ�@�)�@��6@�x@�\�@��@̒�@�/�@ˠ'@���@ʍ�@�7@��@�m]@��p@�H@Ǿw@�33@ƕ�@���@�\�@��B@�q@���@��@��"@��@�҉@�bN@�($@�x@��T@���@�j@�&�@�֡@�y>@�Ta@� �@��T@���@�o @�>�@�q@���@�)�@��+@��a@�o@�y>@�9X@��@���@���@��@���@�j@�B[@�$@���@�a�@�;@���@�<�@��@���@�V@��9@�ݘ@�a�@�0�@��x@�1@��*@�}�@���@���@�$@��[@�Mj@�!-@���@�
�@�l�@�ی@��r@�^�@�@��L@�A�@��@��@���@�v`@�!�@���@�h
@���@�Y@���@�h�@�K^@���@�RT@���@��@���@��@���@��@�{�@�?�@�	@��a@�a�@��2@�w�@�-@��@��#@���@�U�@���@���@��@�E�@���@�y�@�9�@��B@�_�@�	�@��@���@�t�@�Dg@���@��+@��+@���@��{@�`B@��5@��u@�}V@�_@��D@��@���@�o�@�"�@��@��,@�W�@� �@���@��2@���@���@�"h@��j@���@�zx@�?}@���@��@�h
@�:*@���@���@�g�@� \@��@��s@��e@��_@�V@��@��0@���@�g�@�L�@��@��@���@�5?@���@��f@�'�@��E@��@�?@��9@��q@��	@�J#@��@��	@���@�U2@��)@���@�Y�@�=@��@��b@�g8@�$�@���@���@�n/@�H�@���@�`�@�?�@� �@�@��@���@���@�a�@��@��<@���@�V@�	@���@���@���@�f�@�=�@�@��c@���@��D@�M@�:*@�-�@�$@�x@��@�rG@�H�@�-w@�@@���@��y@��E@��h@��}@��Y@�	@���@���@��	@�!-@���@��	@���@��@�!�@�@K�@~��@~�+@~�@}@}�=@}:�@|�@|:�@{��@{'�@z�R@z8�@y��@y2a@x�@x�9@x>B@w>�@v�<@v=q@u��@u�'@uV@t��@tA�@s,�@r�@r�b@rTa@qԕ@q��@qG�@qF@q�@p��@p��@pq@p,=@ot�@o�@n�L@n��@n}V@nGE@m�^@mk�@mN<@m�@lѷ@l1'@k�@k��@k��@k9�@k�@j��@jq�@j!�@i��@iq@h�@h�@hbN@h2�@h�@g��@giD@f�M@f��@fc @fJ�@fGE@fO@e��@e|@e=�@d��@dr�@c��@c��@c��@cx@c+@b��@b��@b	@a�n@ae,@a7L@`��@`<�@_��@_l�@^��@^��@^�@]�=@]IR@\ѷ@\-�@[�F@[C@Z��@Z��@ZM�@Z_@Y��@Y^�@Y+@X�[@X�4@Xy>@XS�@W�g@W�F@W�V@WE9@W@V҉@V��@V6�@U�=@Uo @U[W@U�@T�@T~@S��@Sn/@R�H@R@Qm]@P��@P��@PbN@P�@O�	@O@N��@N^5@N
�@M��@M�M@L��@Lѷ@L�U@L��@LH@K��@K�[@Ko�@KE9@K"�@K�@J��@J}V@J5?@J
�@I�C@If�@IF@H��@H��@H��@H �@G+@F}V@E�@Ezx@E^�@E%@D�5@D��@DbN@D/�@Db@C��@CRT@B��@B��@B��@Bv�@Bn�@Ba|@B^5@B;�@A�@AIR@A	l@@��@@Q�@@-�@@�@?خ@?�@?W?@?!-@>�y@>��@=�N@=|@=�@<�@<%�@;��@;.I@:�@:��@:xl@9�@9�C@9�'@9��@9Vm@8ی@7�&@7�@7��@7qv@7�@6�2@6z@6 �@5��@54@4�@4�u@4(�@3=@2�b@2�@1��@1`B@1;@0��@0��@0��@0�u@0]d@/�]@/�6@/�[@/�P@/�{@/y�@/dZ@.�8@.a|@.GE@.�@-��@-�z@-8�@,�`@,��@,Xy@+��@+�V@+a@++@*͟@*Q@)�@)��@)m]@)F@)�@(Z@(@(1@(  @'y�@'S@&��@&i�@&($@%@%�@%Dg@%!�@%�@$�.@$_@$Q�@$-�@#�m@#�@@#S�@#�@#�@#@"�2@"��@"d�@!��@!�=@!|@!^�@ �@ �e@ �@ w�@ 4n@��@�@�P@@O@Y@�@�,@�x@��@��@Ov@�@�H@\�@��@�@~(@e�@Ft@-�@��@��@~�@=@ߤ@�@��@xl@B[@�@�@��@zx@A @�@�Y@S�@$@��@J#@$t@Y@@��@�2@�@�m@��@��@�@v�@Q@@�@;�@�@J@�o@�@�T@@ \@��@�@?�@G@��@n/@@O@C@�@��@�@�r@Q@�@�>@�T@��@�'@|@J�@+@�@�|@�j@�4@��@m�@�@@�;@K�@.I@�@��@��@ں@��@�x@q�@($@��@�@rG@%F@��@�@m�@x@��@��@��@E9@9�@$t@
�"@
��@
�]@
��@
�@	�@	�@	�~@	X@	<6@	;@��@_@A�@,=@@�+@�g@�*@\)@.I@�@ i@�@�X@��@�+@z11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��vA��A���A���A���A��mA�ǮA�|�A�?�A��AկAչ�Aռ�Aպ�A��)A��A��A�A���A�� A��BA���A���A���A��%A��,A�� A���A���AղaAըXA՜�AՉ�A�u%A�i�A�g�A�a|A�ZA�ZA�XyA�M�A�IA��;A�یAԴ�A���A�w2A�$tA�~�AȍAƹ$A��A��A��OA��A��MA�m]A�'�A�� A�{�A���A��A�x�A��/A�.�A�ZA�t�A�/�A���A�E9A��A��A�(XA��A��A���A��A�{�A��{A��hA���A��A�B'A���A�7A�6�A�;�A��A��qA�"4A�C�A�;Ax�LAv�7At�}ApϫAk;AgVAc(A`�A^S&AZ��AWoiAT@AQ�AM�AL|AIg�AH($AGԕAF�AE��AB<6A:��A5�A24�A/j�A.��A-�CA,�PA,��A+��A)�A'��A'�aA'��A'�A&�AA&J#A%�)A$��A$~A#�KA"�1A!E9A!�A��A��A�A��A��A��A��A��A�A+A��A�zA!�A��A�A9XAzxA�A��A
�AXyA��A�A�RAl�AA�A��AQ�AOA�`A3�A�A��A��A֡A�4A��A�aA��AA'RA��A��AU�A@OA
�&A
*�A
e�A
	�A	�uA	|�A�pAo�A	lA��A�AI�A��A��A#�A �Av`A��Az�A �Ap;A�A ��A ��A ��A ��A �IA �oA rGA�A�TA�FA�AC�A S�@���@��O@��r@�_@��Z@�
�@�{J@��$@��@��`@�kQ@�=@��n@��@��4@�1�@�Q�@�G�@�U2@�}�@�Mj@�p;@�a@@��3@�[W@��@�@�:*@�$@�k�@�\@�ݘ@�C�@���@�s�@�$�@���@�:@�@��@���@��@�]d@�	@�˒@��@��@���@�l"@�-�@߇�@��2@�Ta@��;@��@�l"@۷�@�f�@�;@�,=@٦�@�j@�!�@�u%@ד@ְ!@�)�@�?}@Ԙ_@��@��T@ӊ	@�[W@�Z@�|�@п�@�p;@�Xy@��@� i@Λ�@�)�@��6@�x@�\�@��@̒�@�/�@ˠ'@���@ʍ�@�7@��@�m]@��p@�H@Ǿw@�33@ƕ�@���@�\�@��B@�q@���@��@��"@��@�҉@�bN@�($@�x@��T@���@�j@�&�@�֡@�y>@�Ta@� �@��T@���@�o @�>�@�q@���@�)�@��+@��a@�o@�y>@�9X@��@���@���@��@���@�j@�B[@�$@���@�a�@�;@���@�<�@��@���@�V@��9@�ݘ@�a�@�0�@��x@�1@��*@�}�@���@���@�$@��[@�Mj@�!-@���@�
�@�l�@�ی@��r@�^�@�@��L@�A�@��@��@���@�v`@�!�@���@�h
@���@�Y@���@�h�@�K^@���@�RT@���@��@���@��@���@��@�{�@�?�@�	@��a@�a�@��2@�w�@�-@��@��#@���@�U�@���@���@��@�E�@���@�y�@�9�@��B@�_�@�	�@��@���@�t�@�Dg@���@��+@��+@���@��{@�`B@��5@��u@�}V@�_@��D@��@���@�o�@�"�@��@��,@�W�@� �@���@��2@���@���@�"h@��j@���@�zx@�?}@���@��@�h
@�:*@���@���@�g�@� \@��@��s@��e@��_@�V@��@��0@���@�g�@�L�@��@��@���@�5?@���@��f@�'�@��E@��@�?@��9@��q@��	@�J#@��@��	@���@�U2@��)@���@�Y�@�=@��@��b@�g8@�$�@���@���@�n/@�H�@���@�`�@�?�@� �@�@��@���@���@�a�@��@��<@���@�V@�	@���@���@���@�f�@�=�@�@��c@���@��D@�M@�:*@�-�@�$@�x@��@�rG@�H�@�-w@�@@���@��y@��E@��h@��}@��Y@�	@���@���@��	@�!-@���@��	@���@��@�!�@�@K�@~��@~�+@~�@}@}�=@}:�@|�@|:�@{��@{'�@z�R@z8�@y��@y2a@x�@x�9@x>B@w>�@v�<@v=q@u��@u�'@uV@t��@tA�@s,�@r�@r�b@rTa@qԕ@q��@qG�@qF@q�@p��@p��@pq@p,=@ot�@o�@n�L@n��@n}V@nGE@m�^@mk�@mN<@m�@lѷ@l1'@k�@k��@k��@k9�@k�@j��@jq�@j!�@i��@iq@h�@h�@hbN@h2�@h�@g��@giD@f�M@f��@fc @fJ�@fGE@fO@e��@e|@e=�@d��@dr�@c��@c��@c��@cx@c+@b��@b��@b	@a�n@ae,@a7L@`��@`<�@_��@_l�@^��@^��@^�@]�=@]IR@\ѷ@\-�@[�F@[C@Z��@Z��@ZM�@Z_@Y��@Y^�@Y+@X�[@X�4@Xy>@XS�@W�g@W�F@W�V@WE9@W@V҉@V��@V6�@U�=@Uo @U[W@U�@T�@T~@S��@Sn/@R�H@R@Qm]@P��@P��@PbN@P�@O�	@O@N��@N^5@N
�@M��@M�M@L��@Lѷ@L�U@L��@LH@K��@K�[@Ko�@KE9@K"�@K�@J��@J}V@J5?@J
�@I�C@If�@IF@H��@H��@H��@H �@G+@F}V@E�@Ezx@E^�@E%@D�5@D��@DbN@D/�@Db@C��@CRT@B��@B��@B��@Bv�@Bn�@Ba|@B^5@B;�@A�@AIR@A	l@@��@@Q�@@-�@@�@?خ@?�@?W?@?!-@>�y@>��@=�N@=|@=�@<�@<%�@;��@;.I@:�@:��@:xl@9�@9�C@9�'@9��@9Vm@8ی@7�&@7�@7��@7qv@7�@6�2@6z@6 �@5��@54@4�@4�u@4(�@3=@2�b@2�@1��@1`B@1;@0��@0��@0��@0�u@0]d@/�]@/�6@/�[@/�P@/�{@/y�@/dZ@.�8@.a|@.GE@.�@-��@-�z@-8�@,�`@,��@,Xy@+��@+�V@+a@++@*͟@*Q@)�@)��@)m]@)F@)�@(Z@(@(1@(  @'y�@'S@&��@&i�@&($@%@%�@%Dg@%!�@%�@$�.@$_@$Q�@$-�@#�m@#�@@#S�@#�@#�@#@"�2@"��@"d�@!��@!�=@!|@!^�@ �@ �e@ �@ w�@ 4n@��@�@�P@@O@Y@�@�,@�x@��@��@Ov@�@�H@\�@��@�@~(@e�@Ft@-�@��@��@~�@=@ߤ@�@��@xl@B[@�@�@��@zx@A @�@�Y@S�@$@��@J#@$t@Y@@��@�2@�@�m@��@��@�@v�@Q@@�@;�@�@J@�o@�@�T@@ \@��@�@?�@G@��@n/@@O@C@�@��@�@�r@Q@�@�>@�T@��@�'@|@J�@+@�@�|@�j@�4@��@m�@�@@�;@K�@.I@�@��@��@ں@��@�x@q�@($@��@�@rG@%F@��@�@m�@x@��@��@��@E9@9�@$t@
�"@
��@
�]@
��@
�@	�@	�@	�~@	X@	<6@	;@��@_@A�@,=@@�+@�g@�*@\)@.I@�@ i@�@�X@��@�+@z11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
}�B
~B
}�B
}�B
~]B
~�B
~]B
��B
��B
�yB
��B
�qB
��B
��B
�%B
��B
� B
��B
�$B
�B
�=B
�[B
��B iBEBfB�B�B�BB�B�B�B#BjB B B vB �B �B 'B BB 'B BB�B(�BA�BIlBD�BK�BMPBB�B�4B��B��B��B��B�B��B�0B��B��B��B�B�oB��B�UB�[B�]B�PB�B��B��B�B��B��B��B~�B_�BS�BJXB=qB&fB}B
�B
��B
��B
�&B
��B
x�B
^�B
1'B
�B	��B	�B	��B	�.B	��B	�uB	��B	x�B	eFB	S�B	B�B	2|B	!B	�B	hB	
rB		7B	zB	�B	�B	�B��B��B��B�>B��B�FB�tB�%B�B�B	;B	�B	1�B	8lB	6�B	@iB	>�B	E�B	W�B	\�B	U�B	W�B	\B	c�B	h$B	iyB	h�B	j�B	l=B	m�B	r|B	y�B	x�B	}�B	��B	��B	��B	�\B	�B	��B	�9B	�mB	��B	�B	��B	�	B	��B	��B	��B	�nB	�B	��B	�]B	�oB	��B	�B	��B	żB	�PB	��B	��B	�DB	��B	ɠB	��B	�uB	��B	ȀB	�HB	�VB	�YB	ڠB	یB	��B	�B	ݘB	�]B	��B	�B	�sB	�B	�!B	��B	��B	�IB	��B	�kB	ބB	�B	�B	�	B	�B	�\B	�B	�WB	�B	�B	��B
\B
�B
B

rB
�B	�<B	�`B	�B	��B	��B
�B
xB

�B
B
�B	�wB	�<B
3B
�B
�B
B
 B	��B	��B	�]B	��B	��B	�dB	�DB	�8B	�	B	��B	�^B	�^B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�>B	�B	�ZB	��B	�^B	��B	�B	��B	�^B	�JB	�B	��B	��B	�DB	�rB	��B	��B	�B	��B	��B	�fB	�B	�LB	�zB	��B	�tB	�B	�B	��B	��B	��B	��B	��B	�ZB	�?B	��B	��B	�B	��B	�+B	��B	�`B	��B	��B	�fB	��B	��B	��B	��B	��B	�fB	�RB	��B	��B	��B	�	B	�>B	�rB	�XB	��B	�B	�2B	��B	��B	��B	�B	�`B	��B	��B	��B	��B	�xB	��B	��B	��B	�0B	�JB	�B	��B	�B	�B	��B	��B	��B	��B	�B	��B	��B	�"B	�<B	��B	�]B	�wB	�B	�BB	�(B	��B	�HB
 B
 4B
 B
 B
 OB
 4B
B
�B
�B
�B
�B
�B
�B
�B
MB
�B
B
gB
�B
�B
mB
mB
tB
�B
B
EB
�B
�B
	B
	lB
	�B
	�B
	�B
	�B

#B

�B
�B
JB
JB
dB
�B
~B
�B
�B
�B
�B
�B
PB
�B
PB
�B
"B
"B
�B
(B
�B
�B
B
bB
�B
4B
�B
TB
TB
�B
�B
B
�B
�B
�B
B
�B
�B
B
�B
SB

B
$B
?B
sB
�B
�B
�B
+B
B
�B
�B
�B
�B
�B
�B
�B
B
QB
�B
�B
)B
xB
B
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
/B
�B
B
5B
�B
�B
B
;B
;B
;B
pB
pB
�B
 vB
 BB
 \B
 �B
 �B
 �B
!-B
!|B
!�B
"�B
"�B
#�B
#�B
#�B
$ZB
$tB
$�B
$�B
%,B
%FB
$�B
%B
%�B
&2B
&2B
&B
&2B
&�B
'B
'B
'8B
'8B
'�B
(sB
)B
)�B
+6B
+�B
,�B
-B
-CB
-�B
-�B
./B
.IB
/ B
/5B
/�B
/�B
0;B
0oB
0�B
1AB
1�B
1�B
2GB
2|B
2�B
2�B
2�B
3B
33B
3�B
3�B
4TB
4�B
4�B
4�B
4�B
5B
5?B
5�B
5�B
5�B
6�B
6�B
6�B
6�B
7LB
7LB
72B
7LB
7�B
8B
8lB
8�B
9$B
9	B
9rB
9>B
9XB
9�B
9�B
:*B
:�B
:�B
;0B
;dB
;�B
;�B
;�B
;�B
;�B
<�B
<�B
=<B
=VB
=VB
=�B
=�B
>B
>�B
>�B
>�B
?.B
?}B
?�B
?�B
?�B
@ B
@ B
@B
@4B
@4B
@�B
A B
AoB
AoB
AUB
A�B
BB
B[B
B[B
BuB
B�B
CaB
C{B
C�B
C�B
C�B
C�B
C�B
DMB
D3B
D�B
E9B
ESB
EmB
ESB
EmB
E�B
FYB
F�B
F�B
FtB
G+B
GB
GB
G+B
G�B
G�B
GzB
G�B
H1B
H�B
H�B
H�B
H�B
IB
IlB
I�B
J	B
J=B
JXB
J=B
J=B
J#B
J=B
J#B
JrB
J=B
J�B
J�B
J�B
KDB
K�B
K�B
LdB
LdB
L�B
L�B
L�B
MB
M�B
M�B
M�B
M�B
NB
N"B
N�B
N�B
N�B
O\B
O\B
O\B
O\B
O�B
O�B
O�B
O�B
O�B
P.B
PHB
PHB
P}B
P�B
QB
Q�B
Q�B
Q�B
R B
RoB
S�B
S�B
S�B
SuB
S�B
S�B
S�B
S�B
S�B
S�B
TB
TaB
TaB
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
UB
U�B
U�B
U�B
V�B
W?B
V�B
W
B
W?B
W
B
W�B
W�B
W�B
XB
X_B
X�B
X�B
X�B
X�B
X�B
X�B
YKB
Y�B
ZkB
Z�B
Z�B
Z�B
Z�B
Z�B
[=B
[#B
[	B
Z�B
ZQB
ZkB
ZQB
Z�B
[	B
[#B
[=B
[=B
[�B
\�B
\�B
]~B
]IB
]~B
]dB
]�B
^�B
^�B
_B
_�B
_�B
_�B
_�B
_�B
_�B
`�B
`\B
`BB
`vB
aB
abB
a�B
a�B
a�B
b4B
bNB
b4B
b�B
b�B
b�B
cB
cTB
cnB
c�B
c�B
c�B
c�B
c�B
dB
dZB
dtB
d�B
d�B
d�B
d�B
d�B
e,B
e�B
e�B
e�B
e�B
e�B
f�B
f�B
f�B
gB
g8B
gRB
g�B
g�B
g�B
g�B
h
B
h$B
hXB
hXB
hXB
i*B
iDB
i*B
iB
i�B
i�B
i�B
jB
jKB
jB
j�B
j�B
j�B
j�B
kkB
k�B
k�B
k�B
l"B
l=B
l�B
l�B
l�B
l�B
l�B
mB
l�B
mwB
m�B
m�B
m�B
nIB
n}B
n}B
n}B
n�B
oB
oB
oOB
o�B
o�B
o�B
o�B
p;B
p;B
p;B
pUB
p�B
p�B
qB
q[B
q�B
q�B
q�B
rB
q�B
rGB
rGB
r�B
r�B
r�B
sB
s3B
s3B
shB
s�B
s�B
s�B
s�B
s�B
tB
t�B
t�B
t�B
u?B
u�B
u�B
u�B
u�B
vB
vB
vB
v+B
v+B
vFB
vFB
vzB
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
x8B
xRB
x�B
x�B
y$B
yrB
y�B
y�B
y�B
zB
z*B
zB
zDB
z�B
z�B
z�B
z�B
z�B
{B
{dB
{�B
|PB
|�B
|�B
}<B
}qB
}qB
}�B
}�B
}�B
}�B
}�B
~]B
~�B
~�B
~�B
HB
HB
cB
cB
}B
}B
�B
�B
��B
�iB
��B
��B
�B
�B
� B
��B
�oB
��B
��B
��B
��B
�[B
�B
�B
�GB
�-B
��B
�B
�3B
�B
�SB
��B
�?B
�%B
�?B
�?B
�%B
�?B
�tB
��B
��B
��B
��B
��B
��B
�+11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
}�B
~B
}�B
}�B
~]B
~�B
~]B
��B
��B
�yB
��B
�qB
��B
��B
�%B
��B
� B
��B
�$B
�B
�=B
�[B
��B iBEBfB�B�B�BB�B�B�B#BjB B B vB �B �B 'B BB 'B BB�B(�BA�BIlBD�BK�BMPBB�B�4B��B��B��B��B�B��B�0B��B��B��B�B�oB��B�UB�[B�]B�PB�B��B��B�B��B��B��B~�B_�BS�BJXB=qB&fB}B
�B
��B
��B
�&B
��B
x�B
^�B
1'B
�B	��B	�B	��B	�.B	��B	�uB	��B	x�B	eFB	S�B	B�B	2|B	!B	�B	hB	
rB		7B	zB	�B	�B	�B��B��B��B�>B��B�FB�tB�%B�B�B	;B	�B	1�B	8lB	6�B	@iB	>�B	E�B	W�B	\�B	U�B	W�B	\B	c�B	h$B	iyB	h�B	j�B	l=B	m�B	r|B	y�B	x�B	}�B	��B	��B	��B	�\B	�B	��B	�9B	�mB	��B	�B	��B	�	B	��B	��B	��B	�nB	�B	��B	�]B	�oB	��B	�B	��B	żB	�PB	��B	��B	�DB	��B	ɠB	��B	�uB	��B	ȀB	�HB	�VB	�YB	ڠB	یB	��B	�B	ݘB	�]B	��B	�B	�sB	�B	�!B	��B	��B	�IB	��B	�kB	ބB	�B	�B	�	B	�B	�\B	�B	�WB	�B	�B	��B
\B
�B
B

rB
�B	�<B	�`B	�B	��B	��B
�B
xB

�B
B
�B	�wB	�<B
3B
�B
�B
B
 B	��B	��B	�]B	��B	��B	�dB	�DB	�8B	�	B	��B	�^B	�^B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�>B	�B	�ZB	��B	�^B	��B	�B	��B	�^B	�JB	�B	��B	��B	�DB	�rB	��B	��B	�B	��B	��B	�fB	�B	�LB	�zB	��B	�tB	�B	�B	��B	��B	��B	��B	��B	�ZB	�?B	��B	��B	�B	��B	�+B	��B	�`B	��B	��B	�fB	��B	��B	��B	��B	��B	�fB	�RB	��B	��B	��B	�	B	�>B	�rB	�XB	��B	�B	�2B	��B	��B	��B	�B	�`B	��B	��B	��B	��B	�xB	��B	��B	��B	�0B	�JB	�B	��B	�B	�B	��B	��B	��B	��B	�B	��B	��B	�"B	�<B	��B	�]B	�wB	�B	�BB	�(B	��B	�HB
 B
 4B
 B
 B
 OB
 4B
B
�B
�B
�B
�B
�B
�B
�B
MB
�B
B
gB
�B
�B
mB
mB
tB
�B
B
EB
�B
�B
	B
	lB
	�B
	�B
	�B
	�B

#B

�B
�B
JB
JB
dB
�B
~B
�B
�B
�B
�B
�B
PB
�B
PB
�B
"B
"B
�B
(B
�B
�B
B
bB
�B
4B
�B
TB
TB
�B
�B
B
�B
�B
�B
B
�B
�B
B
�B
SB

B
$B
?B
sB
�B
�B
�B
+B
B
�B
�B
�B
�B
�B
�B
�B
B
QB
�B
�B
)B
xB
B
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
/B
�B
B
5B
�B
�B
B
;B
;B
;B
pB
pB
�B
 vB
 BB
 \B
 �B
 �B
 �B
!-B
!|B
!�B
"�B
"�B
#�B
#�B
#�B
$ZB
$tB
$�B
$�B
%,B
%FB
$�B
%B
%�B
&2B
&2B
&B
&2B
&�B
'B
'B
'8B
'8B
'�B
(sB
)B
)�B
+6B
+�B
,�B
-B
-CB
-�B
-�B
./B
.IB
/ B
/5B
/�B
/�B
0;B
0oB
0�B
1AB
1�B
1�B
2GB
2|B
2�B
2�B
2�B
3B
33B
3�B
3�B
4TB
4�B
4�B
4�B
4�B
5B
5?B
5�B
5�B
5�B
6�B
6�B
6�B
6�B
7LB
7LB
72B
7LB
7�B
8B
8lB
8�B
9$B
9	B
9rB
9>B
9XB
9�B
9�B
:*B
:�B
:�B
;0B
;dB
;�B
;�B
;�B
;�B
;�B
<�B
<�B
=<B
=VB
=VB
=�B
=�B
>B
>�B
>�B
>�B
?.B
?}B
?�B
?�B
?�B
@ B
@ B
@B
@4B
@4B
@�B
A B
AoB
AoB
AUB
A�B
BB
B[B
B[B
BuB
B�B
CaB
C{B
C�B
C�B
C�B
C�B
C�B
DMB
D3B
D�B
E9B
ESB
EmB
ESB
EmB
E�B
FYB
F�B
F�B
FtB
G+B
GB
GB
G+B
G�B
G�B
GzB
G�B
H1B
H�B
H�B
H�B
H�B
IB
IlB
I�B
J	B
J=B
JXB
J=B
J=B
J#B
J=B
J#B
JrB
J=B
J�B
J�B
J�B
KDB
K�B
K�B
LdB
LdB
L�B
L�B
L�B
MB
M�B
M�B
M�B
M�B
NB
N"B
N�B
N�B
N�B
O\B
O\B
O\B
O\B
O�B
O�B
O�B
O�B
O�B
P.B
PHB
PHB
P}B
P�B
QB
Q�B
Q�B
Q�B
R B
RoB
S�B
S�B
S�B
SuB
S�B
S�B
S�B
S�B
S�B
S�B
TB
TaB
TaB
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
UB
U�B
U�B
U�B
V�B
W?B
V�B
W
B
W?B
W
B
W�B
W�B
W�B
XB
X_B
X�B
X�B
X�B
X�B
X�B
X�B
YKB
Y�B
ZkB
Z�B
Z�B
Z�B
Z�B
Z�B
[=B
[#B
[	B
Z�B
ZQB
ZkB
ZQB
Z�B
[	B
[#B
[=B
[=B
[�B
\�B
\�B
]~B
]IB
]~B
]dB
]�B
^�B
^�B
_B
_�B
_�B
_�B
_�B
_�B
_�B
`�B
`\B
`BB
`vB
aB
abB
a�B
a�B
a�B
b4B
bNB
b4B
b�B
b�B
b�B
cB
cTB
cnB
c�B
c�B
c�B
c�B
c�B
dB
dZB
dtB
d�B
d�B
d�B
d�B
d�B
e,B
e�B
e�B
e�B
e�B
e�B
f�B
f�B
f�B
gB
g8B
gRB
g�B
g�B
g�B
g�B
h
B
h$B
hXB
hXB
hXB
i*B
iDB
i*B
iB
i�B
i�B
i�B
jB
jKB
jB
j�B
j�B
j�B
j�B
kkB
k�B
k�B
k�B
l"B
l=B
l�B
l�B
l�B
l�B
l�B
mB
l�B
mwB
m�B
m�B
m�B
nIB
n}B
n}B
n}B
n�B
oB
oB
oOB
o�B
o�B
o�B
o�B
p;B
p;B
p;B
pUB
p�B
p�B
qB
q[B
q�B
q�B
q�B
rB
q�B
rGB
rGB
r�B
r�B
r�B
sB
s3B
s3B
shB
s�B
s�B
s�B
s�B
s�B
tB
t�B
t�B
t�B
u?B
u�B
u�B
u�B
u�B
vB
vB
vB
v+B
v+B
vFB
vFB
vzB
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
x8B
xRB
x�B
x�B
y$B
yrB
y�B
y�B
y�B
zB
z*B
zB
zDB
z�B
z�B
z�B
z�B
z�B
{B
{dB
{�B
|PB
|�B
|�B
}<B
}qB
}qB
}�B
}�B
}�B
}�B
}�B
~]B
~�B
~�B
~�B
HB
HB
cB
cB
}B
}B
�B
�B
��B
�iB
��B
��B
�B
�B
� B
��B
�oB
��B
��B
��B
��B
�[B
�B
�B
�GB
�-B
��B
�B
�3B
�B
�SB
��B
�?B
�%B
�?B
�?B
�%B
�?B
�tB
��B
��B
��B
��B
��B
��B
�+11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105249  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604192839  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604192840  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604192840                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605042847  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605042847  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610161505                      G�O�G�O�G�O�                