CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:22:05Z creation;2022-06-04T19:22:06Z conversion to V3.1      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604192205  20220610151509  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               AA   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @�HңEg�1   @�H�@0l1&�y�c��\)1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @   @�  @�  A   A   A@  A`  A�  A�  A�33A�  A�  A���A���A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BV  B_��Bh  Bp  Bx  B�  B�  B�  B�33B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�33B�ffB�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  CffC��C  C   C"  C$�C%�fC(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>L�C@  CA�fCD  CF  CH  CJ  CL  CN  CO�fCR  CT  CV  CX  CZ  C\�C^33C_��Ca�fCd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM�fDNfDN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր Dּ�D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�@q�@���@���Az�A<z�A\z�A|z�A�=qA�p�A�=qA�=qA�
>A�
>A�=qA�=qB�B�B�B�B'�B/�B7�B?�BG�BO�BU�B^�RBg�Bo�Bw�B�B��\B��\B�B�B�\)B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\BÏ\BǏ\Bˏ\BϏ\Bӏ\B׏\Bۏ\Bߏ\B�\B�B�B�B���B��\B��\B��\CǮCǮCǮCǮC	ǮCǮCǮCǮCǮCǮCǮCǮC.C�{CǮCǮC!ǮC#�HC%�C'ǮC)ǮC+ǮC-ǮC/ǮC1ǮC3ǮC5ǮC7ǮC9ǮC;ǮC>{C?ǮCA�CCǮCEǮCGǮCIǮCKǮCMǮCO�CQǮCSǮCUǮCWǮCYǮC[�HC]��C_aHCa�CcǮCeǮCgǮCiǮCkǮCmǮCoǮCqǮCsǮCuǮCwǮCyǮC{ǮC}ǮCǮC���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D q�D ��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��D	q�D	��D
q�D
��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��DxRD��Dq�D��Dq�D��Dq�D��Dq�D��D q�D ��D!q�D!��D"q�D"��D#q�D#��D$q�D$��D%q�D%��D&q�D&��D'q�D'��D(q�D(��D)q�D)��D*q�D*��D+q�D+��D,q�D,��D-q�D-��D.q�D.��D/q�D/��D0q�D0��D1q�D1��D2q�D2��D3q�D3��D4q�D4��D5q�D5��D6q�D6��D7q�D7��D8q�D8��D9q�D9��D:q�D:��D;q�D;��D<q�D<��D=q�D=��D>q�D>��D?q�D?��D@q�D@��DAq�DA��DBq�DB��DCq�DC��DDq�DD��DEq�DE��DFq�DF��DGq�DG��DHq�DH��DIq�DI��DJq�DJ��DKq�DK��DLq�DL��DMxRDM�RDNq�DN��DOq�DO��DPq�DP��DQq�DQ��DRq�DR��DSq�DS��DTq�DT��DUq�DU��DVq�DV��DWq�DW��DXq�DX��DYq�DY��DZq�DZ��D[q�D[��D\q�D\��D]q�D]��D^q�D^��D_q�D_��D`q�D`��Daq�Da��Dbq�Db��Dcq�Dc��Ddq�Dd��Deq�De��Dfq�Df��Dgq�Dg��Dhq�Dh��Diq�Di��Djq�Dj��Dkq�Dk��Dlq�Dl��Dmq�Dm��Dnq�Dn��Doq�Do��Dpq�Dp��Dqq�Dq��Drq�Dr��Dsq�Ds��Dtq�Dt��Duq�Du��Dvq�Dv��Dwq�Dw��Dxq�Dx��Dyq�Dy��Dzq�Dz��D{q�D{��D|q�D|��D}q�D}��D~q�D~��Dq�D��D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�|)D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�<)D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D¸�D���D�8�D�x�Dø�D���D�8�D�x�Dĸ�D���D�8�D�x�DŸ�D���D�8�D�x�DƸ�D���D�8�D�x�DǸ�D���D�8�D�x�Dȸ�D���D�8�D�x�Dɸ�D���D�8�D�x�Dʸ�D���D�8�D�x�D˸�D���D�8�D�x�D̸�D���D�8�D�x�D͸�D���D�8�D�x�Dθ�D���D�8�D�x�Dϸ�D���D�8�D�x�Dи�D���D�8�D�x�DѸ�D���D�8�D�x�DҸ�D���D�8�D�x�DӸ�D���D�8�D�x�DԸ�D���D�8�D�x�Dո�D���D�8�D�x�Dֵ�D���D�8�D�x�D׸�D���D�8�D�x�Dظ�D���D�8�D�x�Dٸ�D���D�8�D�x�Dڸ�D���D�8�D�x�D۸�D���D�8�D�x�Dܸ�D���D�8�D�x�Dݸ�D���D�8�D�x�D޸�D���D�8�D�x�D߸�D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D���D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 AՈ�Aՙ�A՝�A՚�A՝�AէAեzAա�Aէ�Aը�AլAի6Aծ}Aկ�Aհ�Aձ�AծIAկ�Aհ�AհUAկOAկ�Aհ�Aլ=A՞�AՁ;A��A�T,A�8�A�P�Aķ�A�&�A�oA�ҽA�lWA�	lA���A��A�!�A�AUA��>A�:�A�e,A�� A�l�A�.}A���A��A�[�A��DA���A��~A�l"A��]A��
A�>wA��fA�ƨA���A���A��PA�M�A��DA��UA�`A�7A��[A�YA�cTA���A��OA��A��A�چA�=�A��A���A�U�A� iA�P�A��A��A���A�LdA���A��A��ZA�B[A��A�ҽA�1�A��oA���A� \A�D3A��A{VmAs��Ap�AmcAc�A\+AX��AV�~AU�AS_AQ!-AL�AK!AJf�AG�AD��A?��A?��A@uA<�LA9e�A7�QA6RTA5��A4��A3`�A2$A1�A2IRA2�A2��A1��A2\�A25?A2�A1l�A0`BA/�QA-��A+�4A(��A(� A(��A(j�A(�A'�8A&�4A$�mA$4A$tTA$�wA#��A!�wA��A�AOvA^5A>�A!�A�A�A;�A��A�aA$tAAn�A\�AC-A�A#�A�A�AN�AخAkQAhsA�IA�AƨATaA��Av�A:�A�A��Aa�Az�A��AZA�DA��AJA�A�6Ao�A=�AZAHArGA�AH�A��A�}A��Ah�A��AO�A�A�vA�ZA�A�QA��Ad�A�9Ap�A�A
�A
,�A
1�A	ƨA	9�A		�A	-A	��A	��A	~�AȴA��A�A��A7�A�A5�A�XA�.Al�A,=AںA.�A<6A�A�A#�A4nAK�Ak�A}VA}�A_AxA �sA ��A ;�A 
�A .�A $�@�o@�� @��@�e,@��1@���@�H�@���@�q@��R@�\�@�ƨ@�IR@�G�@�0�@�&@���@�u@�x�@�E9@��m@��3@�!-@��@�@��@�@@�1'@�X@��@�b@��@�)_@쎊@�^@�J�@�w@�o@薼@�M@�'@�Q�@�Ɇ@�s�@�3�@�@�W?@�oi@㫟@�-w@���@��`@�e�@��@�Vm@��@���@�6@�@��@ܬ@��@�6z@��8@ל�@�@@�J@���@Ջ�@�|�@�t�@�`B@�S@�.�@�\�@ҵ�@�%�@���@Ѵ�@ы�@���@�e�@�0U@���@�@λ�@ά@�kQ@�ϫ@�`B@�RT@�Mj@�:�@��"@�V@��@��c@��@��@�Y@��s@�}V@�@˘�@�F�@ʰ!@�Xy@�H@�$@ɲ�@�|@�j@�Y�@�/�@���@Ȟ@�YK@�b@�w2@��@ƾ�@ƍ�@Ŝ�@�Ĝ@�_�@��Z@ñ[@ä@@Ø�@���@¸R@�Xy@�7�@�1'@�)�@��@�� @���@��r@�e@���@�� @�Ov@���@��0@���@�S&@���@��u@���@���@�"h@��6@��4@���@�1�@�p�@�(�@��9@��@�f�@�.I@�(@���@�M@��@�&�@���@�>�@��@���@��@�@O@�'�@��9@�S�@�	@��.@��>@���@�,�@�$t@��@��B@�Z�@��m@���@�B�@��@��@�i�@��@���@�U�@��	@�m�@�6@��@���@�Mj@��!@�K^@�
�@���@���@��@���@�|�@�F@���@�0U@��]@��H@��h@�j�@�4�@�
=@��s@�bN@���@��h@�4�@���@��@�\�@� �@��@�,�@��@�֡@���@�Z@���@���@�j@�;@��@�%�@�ϫ@��7@�\)@��P@�ی@���@��1@�kQ@��r@�K�@��@���@���@�w�@�v�@�YK@��@��@��h@�j�@��@��R@���@�M@���@��	@���@�@�@��Z@��@��q@�y�@��@���@�7@��6@���@�|�@�7L@���@��@��@���@��@���@�d�@�Ov@�:�@�@�f�@��M@���@�xl@�7@��H@��$@�\)@��p@���@�B[@��3@��M@�Z�@�1�@��@�(@��M@��.@�V@���@�dZ@�?}@�/@� \@� i@��@��@���@��@�� @���@�oi@�0U@��@��@���@��X@���@��4@�E9@��@��[@��u@�h
@�U2@�Ft@�!@�1@�G@���@���@��W@��j@��}@���@���@�S&@���@��@�� @��@�'�@��@��v@���@��u@��+@�xl@�[�@�!�@�
�@���@�o @�l�@�iD@��c@�u%@�_�@�PH@�GE@�K^@�2�@��@��W@�p�@�33@�͟@�oi@�!@خ@RT@>�@"�@~�@~�6@~C�@}�@}�X@}F@|:�@{=@z�c@z�6@z�A@zff@zTa@z5?@y��@yVm@xی@x�@w�g@v��@u�=@uIR@t�?@t-�@s��@s��@sC�@r�R@rff@r6�@r	@q=�@pr�@p7@o�@ol�@o.I@o+@n�X@n?@n5?@n($@ne@m��@m��@m�@lی@l�@l�@lz�@lN�@l/�@l�@k�k@ka@j�}@jQ@j&�@j	@i�D@i��@i&�@h�4@h@g4�@f�!@fq�@fM�@f{@e�@e�@e�C@ec@erG@es�@ek�@e=�@e@d�@d]d@d/�@d�@c��@b��@b{@a�@`]d@_�*@^��@^R�@^;�@^
�@]%F@\��@\1'@[��@[�@Z��@Z{@Y�=@Yc�@Y4@Y	l@Xی@X�Y@X6@X  @W��@W��@WMj@W�@V�h@Uj@U�@T��@S��@R��@R!�@Q�C@Q�~@Qw2@QY�@QY�@QY�@QO�@Q(�@P�@P2�@O��@OY@N��@M��@M^�@M�@L��@L�/@L�e@LA�@K�Q@Kqv@J�@Jd�@J3�@I�@Ic@Ik�@I^�@IDg@I@@H�O@Hg8@G��@GS�@F��@Fi�@E�h@D֡@DtT@D?�@C�]@C��@B�s@BV@B1�@Ac�@@e�@@D�@@4n@?"�@>)�@=�o@=w2@=*0@<�@<�j@<��@<@;�k@;A�@;(@:6�@:$�@:
�@9��@9�M@8C-@8��@82�@8�@8x@8b@8x@7�@7�&@7خ@7�@7��@7�@7��@7�:@7��@7b�@7�@6�@6�6@6z@6	@5�)@5��@5�'@4�K@4�[@4�[@4Ĝ@4�9@4��@4�Y@4w�@4S�@49X@4b@3�
@3�*@3b�@3�@2�,@2$�@1�M@1a�@10�@1(�@1@@1	l@0�P@0�@0�Y@0]d@0M@0%�@/�r@/�6@/�K@/P�@/@.	@-N<@-#�@-q@,��@,��@,V�@,�@+�@+{J@+�@*�B@*��@*�@)��@)`B@)q@(�@(�@(��@(��@(��@(�I@(�@(Z@'��@'qv@'�:@'C@&~�@%�N@%w2@%/@%V@$�p@$��@$��@$�@$e�@$ �@#�@#� @#�q@#��@#1�@#�@"��@"��@"�r@"kQ@"YK@"$�@!�N@!��@!j@ ��@ �@��@t�@6z@�@��@��@\�@8�@��@7L@�/@�j@��@m�@PH@>B@ �@	�@��@�@�{@�}@�@�z@��@��@��@}�@f�@<6@��@�o@1'@�W@�;@�;@ݘ@��@�f@X�@J#@@�X@a|@�@�@��@�-@��@�C@x�@[W@�@��@��@�u@~(@*�@��@��@�{@X�@�h@p;@d�@h
@ff@YK@Ov@@�N11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 AՈ�Aՙ�A՝�A՚�A՝�AէAեzAա�Aէ�Aը�AլAի6Aծ}Aկ�Aհ�Aձ�AծIAկ�Aհ�AհUAկOAկ�Aհ�Aլ=A՞�AՁ;A��A�T,A�8�A�P�Aķ�A�&�A�oA�ҽA�lWA�	lA���A��A�!�A�AUA��>A�:�A�e,A�� A�l�A�.}A���A��A�[�A��DA���A��~A�l"A��]A��
A�>wA��fA�ƨA���A���A��PA�M�A��DA��UA�`A�7A��[A�YA�cTA���A��OA��A��A�چA�=�A��A���A�U�A� iA�P�A��A��A���A�LdA���A��A��ZA�B[A��A�ҽA�1�A��oA���A� \A�D3A��A{VmAs��Ap�AmcAc�A\+AX��AV�~AU�AS_AQ!-AL�AK!AJf�AG�AD��A?��A?��A@uA<�LA9e�A7�QA6RTA5��A4��A3`�A2$A1�A2IRA2�A2��A1��A2\�A25?A2�A1l�A0`BA/�QA-��A+�4A(��A(� A(��A(j�A(�A'�8A&�4A$�mA$4A$tTA$�wA#��A!�wA��A�AOvA^5A>�A!�A�A�A;�A��A�aA$tAAn�A\�AC-A�A#�A�A�AN�AخAkQAhsA�IA�AƨATaA��Av�A:�A�A��Aa�Az�A��AZA�DA��AJA�A�6Ao�A=�AZAHArGA�AH�A��A�}A��Ah�A��AO�A�A�vA�ZA�A�QA��Ad�A�9Ap�A�A
�A
,�A
1�A	ƨA	9�A		�A	-A	��A	��A	~�AȴA��A�A��A7�A�A5�A�XA�.Al�A,=AںA.�A<6A�A�A#�A4nAK�Ak�A}VA}�A_AxA �sA ��A ;�A 
�A .�A $�@�o@�� @��@�e,@��1@���@�H�@���@�q@��R@�\�@�ƨ@�IR@�G�@�0�@�&@���@�u@�x�@�E9@��m@��3@�!-@��@�@��@�@@�1'@�X@��@�b@��@�)_@쎊@�^@�J�@�w@�o@薼@�M@�'@�Q�@�Ɇ@�s�@�3�@�@�W?@�oi@㫟@�-w@���@��`@�e�@��@�Vm@��@���@�6@�@��@ܬ@��@�6z@��8@ל�@�@@�J@���@Ջ�@�|�@�t�@�`B@�S@�.�@�\�@ҵ�@�%�@���@Ѵ�@ы�@���@�e�@�0U@���@�@λ�@ά@�kQ@�ϫ@�`B@�RT@�Mj@�:�@��"@�V@��@��c@��@��@�Y@��s@�}V@�@˘�@�F�@ʰ!@�Xy@�H@�$@ɲ�@�|@�j@�Y�@�/�@���@Ȟ@�YK@�b@�w2@��@ƾ�@ƍ�@Ŝ�@�Ĝ@�_�@��Z@ñ[@ä@@Ø�@���@¸R@�Xy@�7�@�1'@�)�@��@�� @���@��r@�e@���@�� @�Ov@���@��0@���@�S&@���@��u@���@���@�"h@��6@��4@���@�1�@�p�@�(�@��9@��@�f�@�.I@�(@���@�M@��@�&�@���@�>�@��@���@��@�@O@�'�@��9@�S�@�	@��.@��>@���@�,�@�$t@��@��B@�Z�@��m@���@�B�@��@��@�i�@��@���@�U�@��	@�m�@�6@��@���@�Mj@��!@�K^@�
�@���@���@��@���@�|�@�F@���@�0U@��]@��H@��h@�j�@�4�@�
=@��s@�bN@���@��h@�4�@���@��@�\�@� �@��@�,�@��@�֡@���@�Z@���@���@�j@�;@��@�%�@�ϫ@��7@�\)@��P@�ی@���@��1@�kQ@��r@�K�@��@���@���@�w�@�v�@�YK@��@��@��h@�j�@��@��R@���@�M@���@��	@���@�@�@��Z@��@��q@�y�@��@���@�7@��6@���@�|�@�7L@���@��@��@���@��@���@�d�@�Ov@�:�@�@�f�@��M@���@�xl@�7@��H@��$@�\)@��p@���@�B[@��3@��M@�Z�@�1�@��@�(@��M@��.@�V@���@�dZ@�?}@�/@� \@� i@��@��@���@��@�� @���@�oi@�0U@��@��@���@��X@���@��4@�E9@��@��[@��u@�h
@�U2@�Ft@�!@�1@�G@���@���@��W@��j@��}@���@���@�S&@���@��@�� @��@�'�@��@��v@���@��u@��+@�xl@�[�@�!�@�
�@���@�o @�l�@�iD@��c@�u%@�_�@�PH@�GE@�K^@�2�@��@��W@�p�@�33@�͟@�oi@�!@خ@RT@>�@"�@~�@~�6@~C�@}�@}�X@}F@|:�@{=@z�c@z�6@z�A@zff@zTa@z5?@y��@yVm@xی@x�@w�g@v��@u�=@uIR@t�?@t-�@s��@s��@sC�@r�R@rff@r6�@r	@q=�@pr�@p7@o�@ol�@o.I@o+@n�X@n?@n5?@n($@ne@m��@m��@m�@lی@l�@l�@lz�@lN�@l/�@l�@k�k@ka@j�}@jQ@j&�@j	@i�D@i��@i&�@h�4@h@g4�@f�!@fq�@fM�@f{@e�@e�@e�C@ec@erG@es�@ek�@e=�@e@d�@d]d@d/�@d�@c��@b��@b{@a�@`]d@_�*@^��@^R�@^;�@^
�@]%F@\��@\1'@[��@[�@Z��@Z{@Y�=@Yc�@Y4@Y	l@Xی@X�Y@X6@X  @W��@W��@WMj@W�@V�h@Uj@U�@T��@S��@R��@R!�@Q�C@Q�~@Qw2@QY�@QY�@QY�@QO�@Q(�@P�@P2�@O��@OY@N��@M��@M^�@M�@L��@L�/@L�e@LA�@K�Q@Kqv@J�@Jd�@J3�@I�@Ic@Ik�@I^�@IDg@I@@H�O@Hg8@G��@GS�@F��@Fi�@E�h@D֡@DtT@D?�@C�]@C��@B�s@BV@B1�@Ac�@@e�@@D�@@4n@?"�@>)�@=�o@=w2@=*0@<�@<�j@<��@<@;�k@;A�@;(@:6�@:$�@:
�@9��@9�M@8C-@8��@82�@8�@8x@8b@8x@7�@7�&@7خ@7�@7��@7�@7��@7�:@7��@7b�@7�@6�@6�6@6z@6	@5�)@5��@5�'@4�K@4�[@4�[@4Ĝ@4�9@4��@4�Y@4w�@4S�@49X@4b@3�
@3�*@3b�@3�@2�,@2$�@1�M@1a�@10�@1(�@1@@1	l@0�P@0�@0�Y@0]d@0M@0%�@/�r@/�6@/�K@/P�@/@.	@-N<@-#�@-q@,��@,��@,V�@,�@+�@+{J@+�@*�B@*��@*�@)��@)`B@)q@(�@(�@(��@(��@(��@(�I@(�@(Z@'��@'qv@'�:@'C@&~�@%�N@%w2@%/@%V@$�p@$��@$��@$�@$e�@$ �@#�@#� @#�q@#��@#1�@#�@"��@"��@"�r@"kQ@"YK@"$�@!�N@!��@!j@ ��@ �@��@t�@6z@�@��@��@\�@8�@��@7L@�/@�j@��@m�@PH@>B@ �@	�@��@�@�{@�}@�@�z@��@��@��@}�@f�@<6@��@�o@1'@�W@�;@�;@ݘ@��@�f@X�@J#@@�X@a|@�@�@��@�-@��@�C@x�@[W@�@��@��@�u@~(@*�@��@��@�{@X�@�h@p;@d�@h
@ff@YK@Ov@@�N11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	# B	"�B	"�B	"�B	"�B	#nB	#:B	#B	#�B	#nB	#�B	#TB	#�B	#nB	#�B	#nB	#B	#TB	#nB	# B	#�B	$B	#�B	'B	0!B	=<B
B
B	ݘB	�EB	�)B	�IB	�B	��B	خB	یB	ܒB	��B	�B	�:B	�kB	�B	��B
 �B
B
�B
�B
 �B
2B
=<B
B�B
I�B
`�B
hXB
jKB
r�B
|�B
��B
�~B
�OB
��B
ڠB
��B
�yB
�}B
�B@B;JBG_BMPBO�BV�BeFBmCBx�B��B�\B��B��B�)B��B�B��BraB`�BT�B@4B�B
��B
�vB
�aB
�aB
��B
q�B
L�B
-�B	��B	��B	��B	{0B	?�B	B	VB	B	oB�B��B��B�IBٴBԯB՛B֡B	�B	B[B	.�B	�B	�B	!�B	9	B	9�B	3�B	9>B	@ B	M�B	i*B	�[B	��B	��B	�|B	��B	�-B	�B	�0B	�kB	��B	x�B	�EB	�!B	��B	��B	�]B	�B	�WB	�TB	��B	��B	��B	��B	��B	��B	�B	�iB	��B	��B	�jB	��B	�DB	�B	�|B	��B	��B	�hB	��B	��B	��B	��B	�PB	��B	��B	�B	�FB	��B	�B	�LB
 4B
[B
{B
[B
-B
�B
EB
�B
,B
�B
xB
xB
"�B
'8B
(�B
(sB
(XB
)�B
,�B
.IB
1B
/�B
*�B
&�B
�B
9B
�B
�B

	B
�B
bB
B
�B
�B
dB
~B
�B
!�B
!�B
"�B
#�B
'�B
(sB
$ZB
$�B
(sB
3�B
4nB
7�B
9�B
9	B
7�B
6zB
3�B
./B
+�B
'B
"4B
jB
B
KB
SB
oB
TB
�B
B
�B
�B
/B
�B
�B
pB
 �B
"4B
!B
�B
�B
!�B
"�B
�B
�B
B
	B
B
�B
�B
�B
�B
sB
�B
$B
�B
�B
#B
xB
�B
dB
]B
qB
	B
�B
B
eB
�B
yB
YB
YB
mB
B
2B
FB
B
:B
�B
�B
�B
�B
�B
�B

=B
	�B
	�B
	�B
	B
1B
+B
�B
�B
�B
9B
SB
�B
�B
	�B
B
DB
0B

XB
�B
GB
�B
aB
-B
AB
B
�B
�B
�B
�B
�B
UB
oB
�B
 �B
 B	��B	��B	��B	�HB
  B	��B	��B	�B	��B	�BB	�]B	�B	��B	��B	��B	��B	�B	��B
'B
�B
�B
�B
tB
�B
�B
	�B
	RB
1B
+B
�B
�B
�B
mB
�B
�B
�B
�B
�B
zB
�B
YB
�B
�B
�B
B
�B
�B
 �B
 B	��B	��B	��B	�}B
 4B
  B
�B
�B
�B
�B
�B
�B
[B
�B
-B
MB
�B
B
B
mB
�B
mB
�B
1B
zB
�B
+B
B
�B
�B
B
mB
mB
�B
�B
�B
�B
�B
_B
�B
�B
	B
	�B
�B
	lB
	7B

rB
�B
VB
VB
�B
�B
�B
�B
�B
vB
\B
BB
�B
�B
BB
�B
�B
.B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
jB
jB
6B
B
jB
PB
jB
pB
�B
�B
BB
\B
vB
�B
�B
�B
}B
�B
�B
 B
4B
hB
�B
�B
�B
�B
 B
B
�B
 B
�B
�B
�B
�B
�B
�B
�B
sB
sB
yB
yB
�B
�B
�B
�B
KB
B
KB
KB
�B
�B
kB
�B
	B
#B
#B
qB
�B
�B
�B
�B
kB
�B
#B
�B
�B
�B
)B
CB
�B
dB
�B
�B
B
B
�B
�B
�B
jB
�B
�B
!B
B
�B
�B
�B
�B
�B
jB
jB
OB
B
OB
VB
B
�B
�B
 vB
 BB
 BB
 \B
 'B
 vB
 �B
 �B
!B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
"B
"�B
#B
#�B
#nB
#�B
#�B
#�B
#�B
#�B
$ZB
$�B
$�B
%,B
%FB
%zB
%`B
%�B
%�B
%�B
%�B
&2B
&B
%�B
&B
%�B
%�B
&B
'8B
'�B
'�B
'�B
)B
)B
)*B
)yB
)�B
)yB
)�B
)�B
*0B
*B
*B
*�B
*�B
*eB
+�B
,�B
,�B
-B
-)B
,�B
-)B
-)B
-)B
-�B
-�B
-wB
.}B
.�B
.�B
/ B
/B
/�B
/�B
/�B
0;B
0�B
1B
1B
1B
1�B
2GB
2�B
2�B
2�B
2�B
33B
3�B
4B
49B
4B
4�B
5�B
6�B
6�B
72B
7�B
7�B
7�B
8B
8�B
8�B
8�B
8�B
8�B
9�B
9�B
:*B
:xB
:�B
:xB
:�B
;0B
;B
;B
:�B
;B
;B
;�B
<B
<B
<B
<6B
<jB
<PB
<�B
<�B
<�B
=qB
=�B
=�B
=�B
=�B
=�B
=�B
>BB
>]B
>�B
?B
?.B
?.B
?HB
?}B
?cB
?}B
?}B
?�B
?}B
?�B
?�B
?�B
?�B
@ B
@B
?�B
@4B
@iB
@�B
A;B
A�B
B'B
C-B
C-B
C-B
C-B
C�B
C�B
DMB
D�B
D�B
EmB
E�B
FB
F%B
F?B
FYB
FtB
F�B
GB
GB
G_B
G_B
G�B
G�B
G�B
IB
IB
IB
I�B
J�B
K^B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
LB
L�B
MB
MjB
M�B
NVB
N�B
N�B
N�B
N�B
N�B
N�B
OBB
OvB
PB
PbB
P}B
P�B
QB
QB
Q4B
QB
Q4B
Q�B
Q�B
RB
RoB
R�B
SB
S�B
T,B
TaB
T{B
T{B
T�B
U2B
UgB
UMB
VB
V�B
V�B
VmB
W�B
XB
X+B
X�B
X�B
X�B
X�B
X�B
YeB
YB
Y�B
Y�B
Z�B
ZkB
ZQB
Z�B
Z7B
[�B
Z�B
[�B
[�B
[�B
[�B
[�B
[�B
[�B
[�B
[�B
[�B
[�B
[�B
[�B
[�B
\B
\)B
\CB
\xB
\�B
\�B
\�B
]B
]B
]�B
]�B
]�B
]�B
]�B
]�B
^B
]�B
^B
^B
^5B
^jB
^jB
^�B
^�B
^�B
_�B
`'B
`\B
`BB
`\B
`vB
`BB
`\B
`'B
`�B
`�B
`�B
aB
aB
a-B
`�B
abB
`�B
a-B
`BB
`B
`'B
`\B
`�B
`vB
`�B
`�B
aB
a�B
a�B
a�B
a�B
b�B
c:B
c�B
dB
d@B
dtB
dtB
dtB
d�B
d�B
d�B
e,B
e`B
ffB
f�B
g�B
h
B
h>B
h�B
h�B
iB
i_B
i*B
i_B
iDB
i�B
i�B
i�B
i�B
i�B
jeB
jB
j�B
j�B
j�B
kB
j�B
kB
kkB
kkB
k�B
lB
l"B
l�B
m)B
m]B
mwB
m�B
m�B
m�B
m�B
nB
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o B
o B
n�B
o�B
p;B
pUB
p;B
p;B
p;B
p;B
pB
poB
qB
qAB
q[B
qvB
q[B
q[B
qAB
q[B
q�B
q�B
q�B
q�B
q�B
r|B
r�B
r�B
r�B
sB
r�B
r�B
s3B
s3B
s�B
tB
s�B
tB
tB
t�B
t�B
t�B
uB
t�B
vB
vFB
v+B
v+B
vFB
vFB
vFB
v�B
v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	# B	"�B	"�B	"�B	"�B	#nB	#:B	#B	#�B	#nB	#�B	#TB	#�B	#nB	#�B	#nB	#B	#TB	#nB	# B	#�B	$B	#�B	'B	0!B	=<B
B
B	ݘB	�EB	�)B	�IB	�B	��B	خB	یB	ܒB	��B	�B	�:B	�kB	�B	��B
 �B
B
�B
�B
 �B
2B
=<B
B�B
I�B
`�B
hXB
jKB
r�B
|�B
��B
�~B
�OB
��B
ڠB
��B
�yB
�}B
�B@B;JBG_BMPBO�BV�BeFBmCBx�B��B�\B��B��B�)B��B�B��BraB`�BT�B@4B�B
��B
�vB
�aB
�aB
��B
q�B
L�B
-�B	��B	��B	��B	{0B	?�B	B	VB	B	oB�B��B��B�IBٴBԯB՛B֡B	�B	B[B	.�B	�B	�B	!�B	9	B	9�B	3�B	9>B	@ B	M�B	i*B	�[B	��B	��B	�|B	��B	�-B	�B	�0B	�kB	��B	x�B	�EB	�!B	��B	��B	�]B	�B	�WB	�TB	��B	��B	��B	��B	��B	��B	�B	�iB	��B	��B	�jB	��B	�DB	�B	�|B	��B	��B	�hB	��B	��B	��B	��B	�PB	��B	��B	�B	�FB	��B	�B	�LB
 4B
[B
{B
[B
-B
�B
EB
�B
,B
�B
xB
xB
"�B
'8B
(�B
(sB
(XB
)�B
,�B
.IB
1B
/�B
*�B
&�B
�B
9B
�B
�B

	B
�B
bB
B
�B
�B
dB
~B
�B
!�B
!�B
"�B
#�B
'�B
(sB
$ZB
$�B
(sB
3�B
4nB
7�B
9�B
9	B
7�B
6zB
3�B
./B
+�B
'B
"4B
jB
B
KB
SB
oB
TB
�B
B
�B
�B
/B
�B
�B
pB
 �B
"4B
!B
�B
�B
!�B
"�B
�B
�B
B
	B
B
�B
�B
�B
�B
sB
�B
$B
�B
�B
#B
xB
�B
dB
]B
qB
	B
�B
B
eB
�B
yB
YB
YB
mB
B
2B
FB
B
:B
�B
�B
�B
�B
�B
�B

=B
	�B
	�B
	�B
	B
1B
+B
�B
�B
�B
9B
SB
�B
�B
	�B
B
DB
0B

XB
�B
GB
�B
aB
-B
AB
B
�B
�B
�B
�B
�B
UB
oB
�B
 �B
 B	��B	��B	��B	�HB
  B	��B	��B	�B	��B	�BB	�]B	�B	��B	��B	��B	��B	�B	��B
'B
�B
�B
�B
tB
�B
�B
	�B
	RB
1B
+B
�B
�B
�B
mB
�B
�B
�B
�B
�B
zB
�B
YB
�B
�B
�B
B
�B
�B
 �B
 B	��B	��B	��B	�}B
 4B
  B
�B
�B
�B
�B
�B
�B
[B
�B
-B
MB
�B
B
B
mB
�B
mB
�B
1B
zB
�B
+B
B
�B
�B
B
mB
mB
�B
�B
�B
�B
�B
_B
�B
�B
	B
	�B
�B
	lB
	7B

rB
�B
VB
VB
�B
�B
�B
�B
�B
vB
\B
BB
�B
�B
BB
�B
�B
.B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
jB
jB
6B
B
jB
PB
jB
pB
�B
�B
BB
\B
vB
�B
�B
�B
}B
�B
�B
 B
4B
hB
�B
�B
�B
�B
 B
B
�B
 B
�B
�B
�B
�B
�B
�B
�B
sB
sB
yB
yB
�B
�B
�B
�B
KB
B
KB
KB
�B
�B
kB
�B
	B
#B
#B
qB
�B
�B
�B
�B
kB
�B
#B
�B
�B
�B
)B
CB
�B
dB
�B
�B
B
B
�B
�B
�B
jB
�B
�B
!B
B
�B
�B
�B
�B
�B
jB
jB
OB
B
OB
VB
B
�B
�B
 vB
 BB
 BB
 \B
 'B
 vB
 �B
 �B
!B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
"B
"�B
#B
#�B
#nB
#�B
#�B
#�B
#�B
#�B
$ZB
$�B
$�B
%,B
%FB
%zB
%`B
%�B
%�B
%�B
%�B
&2B
&B
%�B
&B
%�B
%�B
&B
'8B
'�B
'�B
'�B
)B
)B
)*B
)yB
)�B
)yB
)�B
)�B
*0B
*B
*B
*�B
*�B
*eB
+�B
,�B
,�B
-B
-)B
,�B
-)B
-)B
-)B
-�B
-�B
-wB
.}B
.�B
.�B
/ B
/B
/�B
/�B
/�B
0;B
0�B
1B
1B
1B
1�B
2GB
2�B
2�B
2�B
2�B
33B
3�B
4B
49B
4B
4�B
5�B
6�B
6�B
72B
7�B
7�B
7�B
8B
8�B
8�B
8�B
8�B
8�B
9�B
9�B
:*B
:xB
:�B
:xB
:�B
;0B
;B
;B
:�B
;B
;B
;�B
<B
<B
<B
<6B
<jB
<PB
<�B
<�B
<�B
=qB
=�B
=�B
=�B
=�B
=�B
=�B
>BB
>]B
>�B
?B
?.B
?.B
?HB
?}B
?cB
?}B
?}B
?�B
?}B
?�B
?�B
?�B
?�B
@ B
@B
?�B
@4B
@iB
@�B
A;B
A�B
B'B
C-B
C-B
C-B
C-B
C�B
C�B
DMB
D�B
D�B
EmB
E�B
FB
F%B
F?B
FYB
FtB
F�B
GB
GB
G_B
G_B
G�B
G�B
G�B
IB
IB
IB
I�B
J�B
K^B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
LB
L�B
MB
MjB
M�B
NVB
N�B
N�B
N�B
N�B
N�B
N�B
OBB
OvB
PB
PbB
P}B
P�B
QB
QB
Q4B
QB
Q4B
Q�B
Q�B
RB
RoB
R�B
SB
S�B
T,B
TaB
T{B
T{B
T�B
U2B
UgB
UMB
VB
V�B
V�B
VmB
W�B
XB
X+B
X�B
X�B
X�B
X�B
X�B
YeB
YB
Y�B
Y�B
Z�B
ZkB
ZQB
Z�B
Z7B
[�B
Z�B
[�B
[�B
[�B
[�B
[�B
[�B
[�B
[�B
[�B
[�B
[�B
[�B
[�B
[�B
\B
\)B
\CB
\xB
\�B
\�B
\�B
]B
]B
]�B
]�B
]�B
]�B
]�B
]�B
^B
]�B
^B
^B
^5B
^jB
^jB
^�B
^�B
^�B
_�B
`'B
`\B
`BB
`\B
`vB
`BB
`\B
`'B
`�B
`�B
`�B
aB
aB
a-B
`�B
abB
`�B
a-B
`BB
`B
`'B
`\B
`�B
`vB
`�B
`�B
aB
a�B
a�B
a�B
a�B
b�B
c:B
c�B
dB
d@B
dtB
dtB
dtB
d�B
d�B
d�B
e,B
e`B
ffB
f�B
g�B
h
B
h>B
h�B
h�B
iB
i_B
i*B
i_B
iDB
i�B
i�B
i�B
i�B
i�B
jeB
jB
j�B
j�B
j�B
kB
j�B
kB
kkB
kkB
k�B
lB
l"B
l�B
m)B
m]B
mwB
m�B
m�B
m�B
m�B
nB
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o B
o B
n�B
o�B
p;B
pUB
p;B
p;B
p;B
p;B
pB
poB
qB
qAB
q[B
qvB
q[B
q[B
qAB
q[B
q�B
q�B
q�B
q�B
q�B
r|B
r�B
r�B
r�B
sB
r�B
r�B
s3B
s3B
s�B
tB
s�B
tB
tB
t�B
t�B
t�B
uB
t�B
vB
vFB
v+B
v+B
vFB
vFB
vFB
v�B
v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105241  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604192205  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604192206  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604192206                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605042214  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605042214  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610151509                      G�O�G�O�G�O�                