CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2019-09-08T14:00:31Z creation      
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
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \P   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `0   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  0   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �p   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �p   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �P   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ̰   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �0   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �`   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �`   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �`   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �`   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20190908140031  20190908140031  5904057 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  5006                            2B  A   NAVIS_A                         0305                            082713                          863 @��$��1   @��$�b��@6|(�\�d�r� Ĝ1   GPS     Primary sampling: mixed [deep: discrete, shallow: continuous]                                                                                                                                                                                                      �A   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A���B   B  B  B  B��B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C�fC  C  C  C  C  C  C  C�C �C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl�Cn�Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DM��DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D���D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�0 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@o\*@�{@�{A
=A7
=AW
=Aw
=A��A��A��A��A˅AۅA�Q�A��BBBB\)B%B-B5B=BEBMBUB]BeBmBuB}B��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HCp�Cp�Cp�Cp�C	p�Cp�CW
Cp�Cp�Cp�Cp�Cp�Cp�Cp�C�>C�>C!p�C#p�C%p�C'p�C)p�C+p�C-p�C/p�C1p�C3p�C5p�C7p�C9p�C;p�C=p�C?p�CAp�CCp�CEp�CGp�CIp�CKp�CMp�COp�CQp�CSp�CUp�CWp�CYp�C[p�C]p�C_p�Cap�Ccp�Cep�Cgp�Cip�Ck�>Cm�>Cop�Cqp�Csp�Cup�Cwp�Cyp�C{p�C}p�C�>C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��C¸RCøRCĸRCŸRCƸRCǸRCȸRCɸRCʸRC˸RC̸RC͸RCθRCϸRCиRCѸRCҸRCӸRCԸRCոRCָRC׸RCظRCٸRCڸRC۸RCܸRCݸRC޸RC߸RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC��RC�RC�RC�RC�RC�RC�RC��C��RC��RC��RC��RC��C��RC��RC��RC��RC��RC��RD \)D �)D\)D�)D\)D�)D\)D�)D\)D�)D\)D�)D\)D�)D\)D�)D\)D�)D	\)D	�)D
\)D
�)D\)D�)D\)D�)D\)D�)D\)D�)D\)D�)D\)D�)D\)D�)D\)D�)D\)D�)D\)D�)D\)D�)D\)D�)D\)D�)D\)D�)D\)D�)D\)D�)D\)D�)D\)D�)DU�D�)D\)D�)D\)D�)D \)D �)D!\)D!�)D"\)D"�)D#\)D#�)D$\)D$�)D%\)D%�)D&\)D&�)D'\)D'�)D(\)D(�)D)\)D)�)D*\)D*�)D+\)D+�)D,\)D,�)D-\)D-�)D.\)D.�)D/\)D/�)D0\)D0�)D1\)D1�)D2\)D2�)D3\)D3�)D4\)D4�)D5\)D5�)D6\)D6�)D7\)D7�)D8\)D8�)D9\)D9�)D:\)D:�)D;\)D;�)D<\)D<�)D=\)D=�)D>\)D>�)D?\)D?�)D@\)D@�)DA\)DA�)DB\)DB�)DC\)DC�)DD\)DD�)DE\)DE�)DF\)DF�)DG\)DG�)DH\)DH�)DI\)DI�)DJ\)DJ�)DK\)DK�)DL\)DL�)DM\)DM��DN\)DN�)DO\)DO�)DP\)DP�)DQ\)DQ�)DR\)DR�)DS\)DS�)DT\)DT�)DU\)DU�)DV\)DV�)DW\)DW�)DX\)DX�)DY\)DY�)DZ\)DZ�)D[\)D[�)D\\)D\�)D]\)D]�)D^\)D^�)D_\)D_�)D`\)D`�)Da\)Da�)Db\)Db�)Dc\)Dc�)Dd\)Dd�)De\)De�)Df\)Df�)Dg\)Dg�)Dh\)Dh�)Di\)Di�)Dj\)Dj�)Dk\)Dk�)Dl\)Dl�)Dm\)Dm�)Dn\)Dn�)Do\)Do�)Dp\)Dp�)Dq\)Dq�)Dr\)Dr�)Ds\)Ds�)Dt\)Dt�)Du\)Du�)Dv\)Dv�)Dw\)Dw�)Dx\)Dx�)Dy\)Dy�)Dz\)Dz�)D{\)D{�)D|\)D|�)D}\)D}�)D~\)D~�)D\)D�)D�.D�nD��D��D�.D�nD��D��D�.D�nD��D��D�.D�nD��D��D�.D�nD��D��D�.D�nD��D��D�.D�nD��D��D�.D�nD��D��D�.D�nD��D��D�.D�nD��D��D�.D�nD��D��D�.D�nD��D��D�.D�nD��D��D�.D�nD��D��D�.D�nD��D��D�.D�nD��D��D�.D�nD��D��D�.D�nD��D��D�.D�nD��D��D�.D�nD��D��D�.D�nD��D��D�.D�nD��D��D�.D�nD��D��D�.D�nD��D��D�.D�nD��D��D�.D�nD��D��D�.D�nD��D��D�.D�nD��D��D�.D�nD��D��D�.D�nD��D��D�.D�nD��D��D�.D�nD��D��D�.D�nD��D��D�.D�nD��D��D�.D�nD��D��D�.D�nD��D��D�.D�nD��D��D�1GD�nD��GD��D�.D�nD��D��D�.D�nD��D��D�.D�nD��D��D�.D�nD��D��D�.D�nD��D��D�.D�nD��D��D�.D�nD��D��D�.D�nD��D��D�.D�nD��D��D�.D�nD��D��D�.D�nD��D��D�.D�nD��D��D�.D�nD��D��D�.D�nD��D��D�.D�nD��D��D�.D�nD��D��D�.D�nD��D��D�.D�nD��D��D�.D�nD��D��D�.D�nD��D��D�.D�nD��D��D�.D�nD��D��D�.D�nD��D��D�.D�nD��D��D�.D�nD��D��D�.D�nD��D��D�.D�nD���D��D�.D�nD��D��D�.D�nD®D��D�.D�nDîD��D�.D�nDĮD��D�.D�nDŮD��D�.D�nDƮD��D�.D�nDǮD��D�.D�nDȮD��D�.D�nDɮD��D�.D�nDʮD��D�.D�nDˮD��D�.D�nD̮D��D�.D�nDͮD��D�.D�nDήD��D�.D�nDϮD��D�.D�nDЮD��D�.D�nDѮD��D�.D�nDҮD���D�.D�nDӮD��D�.D�nDԮD��D�.D�nDծD��D�.D�nD֮D��D�.D�nD׮D��D�.D�nDخD��D�.D�nDٮD��D�.D�nDڮD��D�.D�nDۮD��D�.D�nDܮD��D�.D�nDݮD��D�.D�nDޮD��D�.D�nD߮D��D�.D�nD�D��D�.D�nD�D��D�.D�nD�D��D�.D�nD�D��D�.D�qGD�D��D�.D�nD�D��D�.D�nD�D��D�.D�nD�D��D�.D�nD�D��D�.D�nD�D���D�.D�nD�D��D�.D�nD�D��D�.D�nD�D��D�.D�nD��D��D�.D�nD�D��D�.D�nD�D��D�.D�nD�D��D�.D�nD�D��D�.D�nD�D��D�.D�nD�D��D�.D�nD��D��D�.D�nD��D��D�.D�nD��D��D�.D�nD��D��GD�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�33A�33A�5?A�5?A�9XA�7LA�9XA�=qA�9XA�5?A�(�A�+A�33A� �A��A� �A��#A�bA�oA�+A�^5AσAΕ�Aʹ9A���A˛�A�r�A���AŋDA�7LA���A�\)A�l�A�C�A�ffA��PA�1A�G�A�G�A�oA�bNA���A�+A��+A��#A�r�A�G�A��/A�9XA��jA��9A��/A��A��A��A��A�/A��A���A��HA�bA���A��#A��/A��A�?}A��9A�E�A�?}A���A�+A��A�ĜA�S�A���A�t�A�hsA�(�A��
A�ZA��A���A��A���A���A�bA���A�-A�-A��7A��PA�{A���A���A���A�n�A��PA�t�A��;A��`A��jA���A��A~bNA|�RAzM�Ax�HAw�hAv�Au�TAut�AtĜAs��As��As|�ArffAp��Am\)AlffAi��Af��Ae;dAc��Aa��A_A^I�A]p�A\jAZ�+AVz�AU
=AT�HAT�\AS�^AR~�AP�AP=qAO�ANv�AL5?AK`BAJ�uAJ$�AI�#AIp�AHM�AG�AF��AD��AD=qAB��AA7LA@-A?�A>�HA<�\A9�mA7��A6�DA6bA5;dA5%A3�wA2JA0��A0VA/�^A.��A.M�A-�
A-+A,M�A*��A)�A(bA&�A%�A%�A$��A$��A#p�A!�PA�A�RA1'AA��AM�A�7A�AS�A^5AA�HA��A��AJAC�A��AAVAG�A(�Ax�AA��A
�A	�7A(�A��AE�A�-A�`AAĜA9XA%A Z@�ƨ@�^5@�Ĝ@���@��H@���@���@��@�!@�X@��@�@��@�v�@���@�h@�7L@�r�@���@��@㝲@��H@�V@�5?@�{@�@�A�@�(�@���@�O�@�t�@�$�@�|�@�M�@���@���@�n�@�@��T@Ѻ^@љ�@�X@��@���@�9X@�+@͑h@ʸR@�@��T@ɩ�@�Z@ǍP@��H@�E�@�G�@�Z@å�@��@�-@�hs@�`B@��@��@���@���@�O�@�?}@�V@��9@�1'@���@�ƨ@�S�@�J@���@�l�@��!@�M�@�-@��h@�%@��j@�7L@��@���@�5?@�-@�bN@���@���@��@��w@�dZ@���@�p�@�J@���@�9X@��P@���@��@��\@���@�V@�1'@���@��@�o@�C�@��@�ff@��-@�7L@��@���@��@�Q�@�Q�@��@�1@�bN@��@�/@��@��@���@�Ĝ@�z�@�A�@�9X@�1'@���@���@��+@�-@���@�x�@�%@�A�@�"�@��R@��#@��9@�ƨ@�C�@�v�@�J@��-@��7@��/@�z�@�I�@�9X@��@��;@���@�"�@�V@�5?@�-@�-@�-@�-@�$�@�J@���@�%@���@�j@��@��w@�C�@��@���@�v�@�-@�{@��@�`B@��/@��9@�z�@�A�@�(�@�1@��@�ƨ@���@��P@��@��@�t�@�S�@�K�@�;d@�o@��@�V@��h@�/@�Ĝ@���@��u@��D@�r�@�bN@�Q�@�9X@�(�@��@�  @��m@��F@�;d@�~�@��^@��7@��@��@��@��@�O�@�7L@�7L@�O�@�?}@�?}@���@�bN@���@�l�@�ȴ@�5?@�X@���@���@��D@�r�@�Q�@�1'@���@��m@��
@���@��w@��F@���@���@��@�dZ@���@��\@�5?@��@�J@���@���@��@���@��/@��9@���@��u@�z�@�  @~�@}�T@}O�@|9X@{C�@z��@z�\@zM�@z=q@zJ@y�^@y��@y�7@xĜ@wl�@vȴ@u�@u�@t��@t��@tI�@s�m@s�F@r��@rn�@q��@qx�@q�@q7L@qx�@q��@r-@r~�@r^5@r�@q�@q�@q�#@q��@q�7@qG�@pĜ@p��@p�u@pr�@pQ�@p �@o�@oK�@n�R@nV@n@m�T@m�-@m��@m��@m��@m�@m`B@mO�@mO�@m/@mV@mV@mV@l��@l�@l�j@l��@l9X@l�@k�
@k�@ko@j�!@i�^@h��@h�@h �@g�@g�@g|�@g\)@f��@f�y@f�R@f5?@e/@d��@d�j@d��@dz�@d9X@cƨ@cC�@c"�@b�@b�H@b~�@b�@a�^@aX@a�@`��@`��@`�@`Q�@`b@_�w@_\)@_�@^�+@]�T@]p�@]/@]�@]�@]V@\�/@\�@\��@\��@\��@\��@\��@\z�@\Z@\1@[��@[t�@[S�@[@Z�\@ZJ@YG�@XĜ@Xr�@X  @W�@W��@W��@W;d@V�y@V�+@U�h@T��@T9X@T(�@T�@S�@R^5@RJ@Q�#@Q�7@Q�7@Qx�@Qx�@Qhs@P�`@PA�@P  @O�@Ol�@N�+@N{@M��@L�j@L9X@K�
@K"�@JM�@I�@I�7@Ihs@H�`@H�9@HbN@H1'@G�w@G��@Gl�@Gl�@G\)@F�y@F��@Fff@D��@D1@Co@B��@B��@B��@B�!@B^5@A&�@?�P@?�@>ȴ@>��@>v�@>E�@=�T@=�-@=�h@=�@=`B@=`B@=/@<�@<�j@<I�@;��@:��@:�@9�^@9X@9%@8�9@7��@7\)@7+@7+@7�@7�@6��@6�R@6�+@6v�@6ff@6V@65?@6$�@6@5�@5��@5�-@5@5�-@5�-@5�-@5�-@5�-@5�-@5�-@5��@5�-@5��@5��@5��@5�@5`B@5?}@5/@5?}@5/@4�@4�j@4j@4I�@4(�@3��@3��@3dZ@3S�@3C�@3"�@2�H@2��@2��@2~�@2M�@2�@1��@1��@1�^@1��@0Ĝ@0  @/|�@/�@.�y@.�@.��@-��@-`B@-V@,�/@,�D@,Z@+��@+��@+@*��@*��@*�\@*^5@*=q@*�@)��@)��@)��@)X@( �@'�@'�;@'�;@'�@'�@'�w@'�@'�P@'\)@&��@&�R@&v�@%@%�@%�@$�/@$��@$�j@$�@$��@$j@$9X@$�@#ƨ@#C�@"�@"=q@!��@!hs@!G�@!�@ ��@ �u@ �@ bN@ 1'@   @��@�@��@;d@v�@$�@{@{@�@�@�@�@z�@I�@�@1@�m@�
@ƨ@�@��@�\@M�@�@hs@7L@�`@�9@��@�u@�u@�@�@Q�@b@�w@|�@\)@+@+@
=@
=@��@��@��@��@��@��@��@��@��@�y@�@�@�@ȴ@ȴ@ȴ@ȴ@ȴ@ȴ@ȴ@v�@$�@@�T@�-@�@p�@O�@/@�@�@V@V@�@z�@�@1@�m@�
@ƨ@�F@��@t�@dZ@S�@C�@o@�@�H@�H@�H@�H@��@n�@-@�@�@�#@�^@7L@Ĝ@A�@ �@�@��@|�@K�@;d@�@�y@�+@V@5?@@�h@?}@/@�@��@�@�/@�j@�@z�@(�@�F@��@�@S�@C�@"�@@@
��@
~�@
~�@
n�@
n�@
^5@	�@	��@	x�@	7L@Q�@A�@1'@1'@ �@  @�@��@�w@�@�P@\)@+@+@+@+@+@+@+@�@�@�R@�R11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�33A�33A�5?A�5?A�9XA�7LA�9XA�=qA�9XA�5?A�(�A�+A�33A� �A��A� �A��#A�bA�oA�+A�^5AσAΕ�Aʹ9A���A˛�A�r�A���AŋDA�7LA���A�\)A�l�A�C�A�ffA��PA�1A�G�A�G�A�oA�bNA���A�+A��+A��#A�r�A�G�A��/A�9XA��jA��9A��/A��A��A��A��A�/A��A���A��HA�bA���A��#A��/A��A�?}A��9A�E�A�?}A���A�+A��A�ĜA�S�A���A�t�A�hsA�(�A��
A�ZA��A���A��A���A���A�bA���A�-A�-A��7A��PA�{A���A���A���A�n�A��PA�t�A��;A��`A��jA���A��A~bNA|�RAzM�Ax�HAw�hAv�Au�TAut�AtĜAs��As��As|�ArffAp��Am\)AlffAi��Af��Ae;dAc��Aa��A_A^I�A]p�A\jAZ�+AVz�AU
=AT�HAT�\AS�^AR~�AP�AP=qAO�ANv�AL5?AK`BAJ�uAJ$�AI�#AIp�AHM�AG�AF��AD��AD=qAB��AA7LA@-A?�A>�HA<�\A9�mA7��A6�DA6bA5;dA5%A3�wA2JA0��A0VA/�^A.��A.M�A-�
A-+A,M�A*��A)�A(bA&�A%�A%�A$��A$��A#p�A!�PA�A�RA1'AA��AM�A�7A�AS�A^5AA�HA��A��AJAC�A��AAVAG�A(�Ax�AA��A
�A	�7A(�A��AE�A�-A�`AAĜA9XA%A Z@�ƨ@�^5@�Ĝ@���@��H@���@���@��@�!@�X@��@�@��@�v�@���@�h@�7L@�r�@���@��@㝲@��H@�V@�5?@�{@�@�A�@�(�@���@�O�@�t�@�$�@�|�@�M�@���@���@�n�@�@��T@Ѻ^@љ�@�X@��@���@�9X@�+@͑h@ʸR@�@��T@ɩ�@�Z@ǍP@��H@�E�@�G�@�Z@å�@��@�-@�hs@�`B@��@��@���@���@�O�@�?}@�V@��9@�1'@���@�ƨ@�S�@�J@���@�l�@��!@�M�@�-@��h@�%@��j@�7L@��@���@�5?@�-@�bN@���@���@��@��w@�dZ@���@�p�@�J@���@�9X@��P@���@��@��\@���@�V@�1'@���@��@�o@�C�@��@�ff@��-@�7L@��@���@��@�Q�@�Q�@��@�1@�bN@��@�/@��@��@���@�Ĝ@�z�@�A�@�9X@�1'@���@���@��+@�-@���@�x�@�%@�A�@�"�@��R@��#@��9@�ƨ@�C�@�v�@�J@��-@��7@��/@�z�@�I�@�9X@��@��;@���@�"�@�V@�5?@�-@�-@�-@�-@�$�@�J@���@�%@���@�j@��@��w@�C�@��@���@�v�@�-@�{@��@�`B@��/@��9@�z�@�A�@�(�@�1@��@�ƨ@���@��P@��@��@�t�@�S�@�K�@�;d@�o@��@�V@��h@�/@�Ĝ@���@��u@��D@�r�@�bN@�Q�@�9X@�(�@��@�  @��m@��F@�;d@�~�@��^@��7@��@��@��@��@�O�@�7L@�7L@�O�@�?}@�?}@���@�bN@���@�l�@�ȴ@�5?@�X@���@���@��D@�r�@�Q�@�1'@���@��m@��
@���@��w@��F@���@���@��@�dZ@���@��\@�5?@��@�J@���@���@��@���@��/@��9@���@��u@�z�@�  @~�@}�T@}O�@|9X@{C�@z��@z�\@zM�@z=q@zJ@y�^@y��@y�7@xĜ@wl�@vȴ@u�@u�@t��@t��@tI�@s�m@s�F@r��@rn�@q��@qx�@q�@q7L@qx�@q��@r-@r~�@r^5@r�@q�@q�@q�#@q��@q�7@qG�@pĜ@p��@p�u@pr�@pQ�@p �@o�@oK�@n�R@nV@n@m�T@m�-@m��@m��@m��@m�@m`B@mO�@mO�@m/@mV@mV@mV@l��@l�@l�j@l��@l9X@l�@k�
@k�@ko@j�!@i�^@h��@h�@h �@g�@g�@g|�@g\)@f��@f�y@f�R@f5?@e/@d��@d�j@d��@dz�@d9X@cƨ@cC�@c"�@b�@b�H@b~�@b�@a�^@aX@a�@`��@`��@`�@`Q�@`b@_�w@_\)@_�@^�+@]�T@]p�@]/@]�@]�@]V@\�/@\�@\��@\��@\��@\��@\��@\z�@\Z@\1@[��@[t�@[S�@[@Z�\@ZJ@YG�@XĜ@Xr�@X  @W�@W��@W��@W;d@V�y@V�+@U�h@T��@T9X@T(�@T�@S�@R^5@RJ@Q�#@Q�7@Q�7@Qx�@Qx�@Qhs@P�`@PA�@P  @O�@Ol�@N�+@N{@M��@L�j@L9X@K�
@K"�@JM�@I�@I�7@Ihs@H�`@H�9@HbN@H1'@G�w@G��@Gl�@Gl�@G\)@F�y@F��@Fff@D��@D1@Co@B��@B��@B��@B�!@B^5@A&�@?�P@?�@>ȴ@>��@>v�@>E�@=�T@=�-@=�h@=�@=`B@=`B@=/@<�@<�j@<I�@;��@:��@:�@9�^@9X@9%@8�9@7��@7\)@7+@7+@7�@7�@6��@6�R@6�+@6v�@6ff@6V@65?@6$�@6@5�@5��@5�-@5@5�-@5�-@5�-@5�-@5�-@5�-@5�-@5��@5�-@5��@5��@5��@5�@5`B@5?}@5/@5?}@5/@4�@4�j@4j@4I�@4(�@3��@3��@3dZ@3S�@3C�@3"�@2�H@2��@2��@2~�@2M�@2�@1��@1��@1�^@1��@0Ĝ@0  @/|�@/�@.�y@.�@.��@-��@-`B@-V@,�/@,�D@,Z@+��@+��@+@*��@*��@*�\@*^5@*=q@*�@)��@)��@)��@)X@( �@'�@'�;@'�;@'�@'�@'�w@'�@'�P@'\)@&��@&�R@&v�@%@%�@%�@$�/@$��@$�j@$�@$��@$j@$9X@$�@#ƨ@#C�@"�@"=q@!��@!hs@!G�@!�@ ��@ �u@ �@ bN@ 1'@   @��@�@��@;d@v�@$�@{@{@�@�@�@�@z�@I�@�@1@�m@�
@ƨ@�@��@�\@M�@�@hs@7L@�`@�9@��@�u@�u@�@�@Q�@b@�w@|�@\)@+@+@
=@
=@��@��@��@��@��@��@��@��@��@�y@�@�@�@ȴ@ȴ@ȴ@ȴ@ȴ@ȴ@ȴ@v�@$�@@�T@�-@�@p�@O�@/@�@�@V@V@�@z�@�@1@�m@�
@ƨ@�F@��@t�@dZ@S�@C�@o@�@�H@�H@�H@�H@��@n�@-@�@�@�#@�^@7L@Ĝ@A�@ �@�@��@|�@K�@;d@�@�y@�+@V@5?@@�h@?}@/@�@��@�@�/@�j@�@z�@(�@�F@��@�@S�@C�@"�@@@
��@
~�@
~�@
n�@
n�@
^5@	�@	��@	x�@	7L@Q�@A�@1'@1'@ �@  @�@��@�w@�@�P@\)@+@+@+@+@+@+@+@�@�@�R@�R11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BBBBBBBBBBBBBBBB�BB�FB��B��B�B�Bu�Bs�B�Bx�B�uB�?B�FB�B��B�VB|�B\)BN�BE�BA�BD�BE�BF�BG�BT�B^5B^5BXBYBdZBt�By�Bz�Bw�Bu�Bt�Bq�BgmBffBcTB^5BW
BM�BE�B?}B:^B33B�BB�B�B�BÖB�}B�qB�qB�XB�^B�jB��B�5B�BB�)B�B��BǮB�wB�FB��B��B� BbNBM�B49B �B
�B
��B
ĜB
�B
��B
y�B
o�B
gmB
w�B
~�B
~�B
z�B
o�B
cTB
N�B
B�B
8RB
33B
+B
&�B
0!B
.B
,B
)�B
!�B
�B

=B	��B	�B	�B	��B	�wB	�B	��B	��B	�\B	�B	w�B	dZB	^5B	^5B	\)B	]/B	VB	K�B	H�B	E�B	=qB	33B	1'B	0!B	/B	/B	,B	'�B	.B	)�B	�B	�B	hB	%B	B	1B	%B��B�B�sB�ZB�NB�;B�5B�)B��B��B��B��BŢBÖB��B�qB�^B�FB�'B�B��B��B��B��B��B��B��B�\B�VB�PB�PB�PB�DB�1B�B�B�B}�B}�B|�B|�B{�Bz�By�Bw�Bu�Bp�Bm�Bm�Bl�Bk�BhsBdZBbNBbNBaHBaHBbNBbNBbNBaHBbNBaHBaHBaHBaHB`BB_;B_;B]/B]/B_;B^5B`BB_;B_;B`BB_;B_;B^5B]/B\)B\)B_;B_;B`BB`BBaHBaHBaHB`BB`BBaHBbNBbNBffBgmBgmBgmBk�Bk�Bk�Bk�Bk�Bk�BjBjBjBk�Bl�Bo�Bp�Bp�Bp�Br�Bt�Bv�Bw�Bx�B{�B|�B}�B~�B~�B}�B|�B~�B~�B�B�B�B�B�B�B�%B�+B�+B�VB�oB�{B��B��B��B��B��B��B��B��B�B�?B��BĜB�}B�}BÖB��B��B�)B�NB�mB�sB�`B�TB�BB�;B�;B�5B�BB�ZB�mB�B�B�B�B�B�B�B�B��B��B��B��B	  B	B		7B	VB	uB	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	!�B	!�B	"�B	#�B	%�B	&�B	+B	,B	/B	0!B	2-B	6FB	<jB	>wB	A�B	B�B	G�B	J�B	K�B	K�B	K�B	L�B	L�B	N�B	S�B	VB	VB	VB	VB	VB	VB	W
B	XB	]/B	^5B	`BB	dZB	e`B	hsB	jB	jB	l�B	n�B	o�B	o�B	t�B	x�B	y�B	{�B	}�B	~�B	� B	�B	�B	�B	�B	�B	�B	�B	�%B	�+B	�+B	�1B	�=B	�VB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�-B	�LB	�jB	�wB	�wB	��B	��B	��B	��B	�}B	��B	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�
B	�B	�B	�#B	�#B	�#B	�)B	�)B	�)B	�)B	�5B	�BB	�HB	�TB	�ZB	�`B	�fB	�fB	�fB	�fB	�fB	�fB	�fB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B
B
B
B
%B
1B
	7B

=B

=B

=B

=B
DB
DB
JB
PB
PB
PB
PB
PB
VB
VB
\B
bB
bB
hB
hB
hB
hB
hB
hB
hB
hB
hB
hB
hB
hB
hB
hB
oB
oB
oB
oB
uB
uB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
"�B
"�B
#�B
#�B
%�B
&�B
&�B
(�B
(�B
)�B
)�B
)�B
)�B
+B
+B
,B
,B
-B
-B
.B
.B
/B
/B
/B
/B
/B
/B
/B
/B
/B
/B
0!B
0!B
0!B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
2-B
2-B
33B
49B
49B
49B
49B
49B
49B
49B
5?B
5?B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
6FB
6FB
6FB
6FB
6FB
6FB
7LB
7LB
6FB
8RB
9XB
9XB
;dB
;dB
;dB
<jB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
@�B
@�B
@�B
C�B
C�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
H�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
T�B
T�B
T�B
T�B
VB
VB
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
XB
XB
XB
XB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
\)B
\)B
\)B
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
^5B
^5B
_;B
_;B
`BB
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
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
e`B
ffB
gmB
gmB
gmB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
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
jB
jB
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
k�B
jB
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
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
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BBBBBBBBBBBBBBBB�BB�FB��B��B�B�Bu�Bs�B�Bx�B�uB�?B�FB�B��B�VB|�B\)BN�BE�BA�BD�BE�BF�BG�BT�B^5B^5BXBYBdZBt�By�Bz�Bw�Bu�Bt�Bq�BgmBffBcTB^5BW
BM�BE�B?}B:^B33B�BB�B�B�BÖB�}B�qB�qB�XB�^B�jB��B�5B�BB�)B�B��BǮB�wB�FB��B��B� BbNBM�B49B �B
�B
��B
ĜB
�B
��B
y�B
o�B
gmB
w�B
~�B
~�B
z�B
o�B
cTB
N�B
B�B
8RB
33B
+B
&�B
0!B
.B
,B
)�B
!�B
�B

=B	��B	�B	�B	��B	�wB	�B	��B	��B	�\B	�B	w�B	dZB	^5B	^5B	\)B	]/B	VB	K�B	H�B	E�B	=qB	33B	1'B	0!B	/B	/B	,B	'�B	.B	)�B	�B	�B	hB	%B	B	1B	%B��B�B�sB�ZB�NB�;B�5B�)B��B��B��B��BŢBÖB��B�qB�^B�FB�'B�B��B��B��B��B��B��B��B�\B�VB�PB�PB�PB�DB�1B�B�B�B}�B}�B|�B|�B{�Bz�By�Bw�Bu�Bp�Bm�Bm�Bl�Bk�BhsBdZBbNBbNBaHBaHBbNBbNBbNBaHBbNBaHBaHBaHBaHB`BB_;B_;B]/B]/B_;B^5B`BB_;B_;B`BB_;B_;B^5B]/B\)B\)B_;B_;B`BB`BBaHBaHBaHB`BB`BBaHBbNBbNBffBgmBgmBgmBk�Bk�Bk�Bk�Bk�Bk�BjBjBjBk�Bl�Bo�Bp�Bp�Bp�Br�Bt�Bv�Bw�Bx�B{�B|�B}�B~�B~�B}�B|�B~�B~�B�B�B�B�B�B�B�%B�+B�+B�VB�oB�{B��B��B��B��B��B��B��B��B�B�?B��BĜB�}B�}BÖB��B��B�)B�NB�mB�sB�`B�TB�BB�;B�;B�5B�BB�ZB�mB�B�B�B�B�B�B�B�B��B��B��B��B	  B	B		7B	VB	uB	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	!�B	!�B	"�B	#�B	%�B	&�B	+B	,B	/B	0!B	2-B	6FB	<jB	>wB	A�B	B�B	G�B	J�B	K�B	K�B	K�B	L�B	L�B	N�B	S�B	VB	VB	VB	VB	VB	VB	W
B	XB	]/B	^5B	`BB	dZB	e`B	hsB	jB	jB	l�B	n�B	o�B	o�B	t�B	x�B	y�B	{�B	}�B	~�B	� B	�B	�B	�B	�B	�B	�B	�B	�%B	�+B	�+B	�1B	�=B	�VB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�-B	�LB	�jB	�wB	�wB	��B	��B	��B	��B	�}B	��B	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�
B	�B	�B	�#B	�#B	�#B	�)B	�)B	�)B	�)B	�5B	�BB	�HB	�TB	�ZB	�`B	�fB	�fB	�fB	�fB	�fB	�fB	�fB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B
B
B
B
%B
1B
	7B

=B

=B

=B

=B
DB
DB
JB
PB
PB
PB
PB
PB
VB
VB
\B
bB
bB
hB
hB
hB
hB
hB
hB
hB
hB
hB
hB
hB
hB
hB
hB
oB
oB
oB
oB
uB
uB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
"�B
"�B
#�B
#�B
%�B
&�B
&�B
(�B
(�B
)�B
)�B
)�B
)�B
+B
+B
,B
,B
-B
-B
.B
.B
/B
/B
/B
/B
/B
/B
/B
/B
/B
/B
0!B
0!B
0!B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
2-B
2-B
33B
49B
49B
49B
49B
49B
49B
49B
5?B
5?B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
6FB
6FB
6FB
6FB
6FB
6FB
7LB
7LB
6FB
8RB
9XB
9XB
;dB
;dB
;dB
<jB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
@�B
@�B
@�B
C�B
C�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
H�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
T�B
T�B
T�B
T�B
VB
VB
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
XB
XB
XB
XB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
\)B
\)B
\)B
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
^5B
^5B
_;B
_;B
`BB
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
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
e`B
ffB
gmB
gmB
gmB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
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
jB
jB
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
k�B
jB
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
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
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.56 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20190908140031                              AO  ARCAADJP                                                                    20190908140031    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20190908140031  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20190908140031  QCF$                G�O�G�O�G�O�0               