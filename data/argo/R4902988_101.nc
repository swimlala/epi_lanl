CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-07-20T09:43:54Z creation;2022-07-20T09:43:55Z conversion to V3.1      
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
_FillValue                 �  I8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tx   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �0   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �p   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �(   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ޘ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �    HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �D   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �T   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �X   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �h   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �l   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �p   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20220720094354  20220720095936  4902988                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               eA   JA                                  2B  A   APEX                            8600                            2.11.3                          846 @���]���1   @��أ�
=@;��E����c��S��1   GPS     A   A   F   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @���@�  A   A!��A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B ffBffB  B��B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B���B�  B�  B�33B�  B�  C   C  C  C  C�fC
  C  C  C  C�fC  C�fC  C  C�fC  C �C"  C$  C%�fC(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Ca�fCc�fCf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~�C�  C�  C��3C�  C��3C��C�  C�  C��3C�  C�  C�  C��C��C�  C�  C�  C�  C��C�  C�  C�  C�  C��3C��C�  C��3C�  C�  C��3C��3C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��3C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��C��C��C��C��C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  D   D � D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D�fDfD� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D fD � D!  D!y�D"  D"� D#  D#� D$  D$y�D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4fD4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@fD@� DA  DA� DA��DBy�DC  DC� DD  DD� DEfDE� DF  DF� DG  DG� DH  DH�fDIfDI�fDJ  DJ� DK  DK� DL  DL� DM  DM� DM��DN� DO  DO� DP  DP� DQ  DQ�fDR  DR� DS  DS� DTfDT� DU  DU� DV  DV� DW  DW�fDXfDX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]�fD^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di�fDj  Djy�Dk  Dk�fDlfDl� Dm  Dm� Dn  Dny�Do  Do� Dp  Dp� Dq  Dq�fDr  Dr� Ds  Ds� Dt  Dt� Dt��Duy�Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~y�D~��D� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D��3D�  D�@ D�� D��3D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�C3D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D��3D�� D�  D�@ D�� D�� D�  D�<�D�|�D���D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D���D�  D�<�D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D���D�@ D�� D�� D�  D�C3D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ D�|�D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�C3Dǀ D�� D�  D�@ DȀ Dȼ�D�  D�@ Dɀ D�� D�  D�@ Dʀ D��3D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�C3D̀ D�� D�3D�C3D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D��3D�  D�@ DԀ D�� D�3D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڃ3D��3D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D޼�D�  D�@ D߀ D�� D�3D�@ D�|�D��D�  D�C3D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�<�D�|�D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D��3D�3D�@ D� D�� D�  D�@ D�� D��D�  D�C3D� D�� D�  D�@ D� D�� D�  D�@ D�|�D��D�  D�@ D� D���D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��H@�{@�{A ��A?
=A_
=A
=A��A��A��A��AυA߅A�B (�B(�BB\)BB'B/B7B?BGBOBWB_BgBoBwBB��HB��HB��HB�{B��HB��HB��HB��HB��HB��B��HB��HB��HB��HB��HB��HB��HB��HBˮB��HB��HB��HB��HB��HB��HB�B��HB��HB�{B��HB��HB��HC�C�C�C�
C	�C�C�C�C�
C�C�
C�C�C�
C�C 
>C!�C#�C%�
C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�
Cc�
Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C~
>C�C��RC��C��RC��C�C��RC��RC��C��RC��RC��RC�C�C��RC��RC��RC��RC�C��RC��RC��RC��RC��C�C��RC��C��RC��RC��C��C��RC��RC��RC��C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC�C��RC��RC��C��RC��RC��RC��RC��RC��C��RC��RC��RC��RC��RC��RC��RC��RC�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��C��C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��C��C��RC��RC��RC��RC�C�C��RC��RC��RC��RC��RC��C��RC��RC��RC��RC��RC�C�C�C�C�C��RC��RC��RC��RC��RC��C��C��RC��RC��RC��RD |)D �)D|)D�)D|)D�)D|)D�)D��D�)D|)D�)D|)D�)D|)D�)D|)D�)D	|)D	�)D
|)D
�)D|)D�)D��D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D��D�)D|)D�)D|)D�)D��D�D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D��D|)D�)D|)D�)D|)D �D |)D �)D!u�D!�)D"|)D"�)D#|)D#�)D$u�D$�)D%|)D%�)D&|)D&�)D'|)D'�)D(|)D(�)D)|)D)�)D*|)D*�)D+|)D+�)D,|)D,�)D-|)D-�)D.|)D.�)D/|)D/�)D0|)D0�)D1|)D1�)D2|)D2�)D3|)D4�D4|)D4�)D5|)D5�)D6|)D6�)D7|)D7�)D8|)D8�)D9|)D9�)D:|)D:�)D;|)D;�)D<|)D<�)D=|)D=�)D>|)D>�)D?|)D@�D@|)D@�)DA|)DA��DBu�DB�)DC|)DC�)DD|)DE�DE|)DE�)DF|)DF�)DG|)DG�)DH��DI�DI��DI�)DJ|)DJ�)DK|)DK�)DL|)DL�)DM|)DM��DN|)DN�)DO|)DO�)DP|)DP�)DQ��DQ�)DR|)DR�)DS|)DT�DT|)DT�)DU|)DU�)DV|)DV�)DW��DX�DX|)DX�)DY|)DY�)DZ|)DZ�)D[|)D[�)D\|)D\�)D]��D]�)D^|)D^�)D_|)D_�)D`|)D`�)Da|)Da�)Db|)Db�)Dc|)Dc�)Dd|)Dd�)De|)De�)Df|)Df�)Dg|)Dg�)Dh|)Dh�)Di��Di�)Dju�Dj�)Dk��Dl�Dl|)Dl�)Dm|)Dm�)Dnu�Dn�)Do|)Do�)Dp|)Dp�)Dq��Dq�)Dr|)Dr�)Ds|)Ds�)Dt|)Dt��Duu�Du�)Dv|)Dv�)Dw|)Dw�)Dx|)Dx�)Dy|)Dy�)Dz|)Dz�)D{|)D{�)D||)D|�)D}|)D}�)D~u�D~��D|)D�)D�>D�~D��D��D�>D�~D��D���D�>D�~D��GD��D�>D�~D��GD��D�:�D�~D��D��D�>D�~D��D��D�>D�~D��D��D�:�D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D��GD��D��D�>D�~D��D��D�AGD��GD��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�AGD��GD��D��D�>D�~D��D��D�:�D�z�D���D��D�>D�~D��D��D�AGD�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�AGD�~D���D��D�:�D�z�D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D���D���D�>D�~D��D��D�AGD��GD��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D���D��D�>D�~D��D��D�>D��GD��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��GD��D�>D�~D��D��D�>D�~D��D��D�AGD�~D��D�GD�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D���D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�AGD�~D��D��D�>D�~D��D��D�>D�~D¾D��D�>D�z�DþD��D�>D�~DľD��D�>D�~DžD��D�>D�~DƾD��D�AGD�~DǾD��D�>D�~DȺ�D��D�>D�~DɾD��D�>D�~D��GD��D�>D�~D˾D��D�>D�~D̾D��D�AGD�~D;D�GD�AGD�~DξD��D�>D�~DϾD��D�>D�~DоD��D�>D�~DѾD��D�>D�~DҾD��D�>D�~D��GD��D�>D�~DԾD�GD�>D�~DվD��D�>D�~D־D��D�>D�~D׾D��D�>D�~DؾD��D�>D�~DپD��D�>DځGD��GD��D�>D�~D۾D��D�>D�~DܾD��D�>D�~DݾD��D�>D�~D޺�D��D�>D�~D߾D�GD�>D�z�D��D��D�AGD�~D�D��D�>D�~D�D�GD�>D�~D�D��D�:�D�z�D�D���D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D�GD�>D�~D��D��D�>D�~D��GD�GD�>D�~D�D��D�>D�~D��D��D�AGD�~D�D��D�>D�~D�D��D�>D�z�D��D��D�>D�~D���D��D�>D�~D��D���D�>D�~D��D��D�>D�~D��D��D�>D�~D��D�GD�>D�~D��D���D�>D�~D��D��D�>D�~D��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A� A�A�$A��A�oA�A�FA�+A�A��A�bA�PA�	�A�A��A��A��sA��]A��/A��?A��?A��,A�F�A�o�A��A�zDA���A�C�A��MA���A�poA�!�A�}�A��JA�=qA�*0A���A�C�A�x8A�T�A��RA��A��lA��A� 'A��	A��XA�:�A��9A���A��A��{A��vA�[�A�ÖA��RA�CaA� �A���A���A��pA�v�A���A���A�7A�M�A��pA�3�A��A�e�A�^�A���A��pA��A��dA���A��6A�  A�jA��xA��QA���A��'A��pA�U�A���A�S[A��aA�{JA���A�L�A�~A�=�A�A��A���A��SA��A���A��xA��TA���A��oA��A}�TA|SA{��A{	lAz[WAy��Ax�MAw��AujAs<6Apz�An>�Am��Al�"Al  AjȴAj�Ail�Ah�MAhB�Ah*0Ah�Ag��Af��Ae�Ad �Ab�wAb�A`��A_<�A]CA\qA[��A[=AZ�FAYxlAW��AV��AUATcAR�KAPݘAPhsAP%FAO'�AN|�AM�QALMAKH�AJ�ZAIںAFoiACԕAB�A@6A>�WA>U2A=�)A=�A<_A;��A9��A7u�A6e�A6�A5kQA4%FA3kQA1�PA/��A.3�A-
�A,:�A+dZA+1�A+CA*�]A*�$A)�$A(��A('�A&��A&�A%�xA%	A$��A$c�A$5?A$A#�}A#FtA"�A!ƨA!L0A ��A -�A�dA��A�'A,=A+A �A�AMA�?A�A�A6�AݘAl"AM�AϫAXyA�]Ah�A;dA>BA��A��A�"AA��A6A
��A
��A
C-A	��A:�A�A�A{AjA�cA�wA|�A�Am]AJAc A�$A}VA&�A�`A�A �6@�Z�@�m]@�Ta@��@��:@��@�ϫ@��$@��@��5@�4@�&�@�@� �@�o�@��@�7�@�C�@�4@�`�@�kQ@���@�e�@�u@��}@�5?@�33@�
�@ݎ"@��,@�{�@ۥ�@��@؏\@��]@װ�@��p@�n�@�o@��@�~�@�@O@��@е@ϕ�@��@�u%@�=@���@Ⱦ@�l"@���@���@İ!@��@_@�@���@�8@��@�K�@�ѷ@���@�h�@�Q�@�M@�9X@��@���@��c@��@�l"@�>�@�/�@�ں@�`�@�&�@��@�F�@��K@�x@���@�e,@�q@���@��}@���@�v`@�>�@��@��A@��P@�	l@�!�@���@���@�N<@�($@�c@�G@�o@���@�oi@�H@���@���@�F�@���@��@�?}@��p@���@�d�@�U2@�Z@�W�@�A�@� �@��>@��@���@�C@��2@��b@�kQ@�]d@��)@�h�@���@���@�Q�@��@���@���@�6@��@���@���@�x�@�j�@�=�@��@�@@�S@�-@�+@��@��@�6�@�ԕ@���@��@�($@�s@���@��4@���@�m�@�Ta@�@�;@��K@���@�l�@�bN@��@���@�q@�Ta@�H�@�:*@�,=@��+@��q@��$@�+@���@�_@��.@���@�G�@���@��<@�.�@���@�_p@�-w@��)@��!@��x@�v�@�V@�C�@�=q@�2�@��@�"h@�@�@�g8@��@�v�@��@���@��@��4@���@���@�\�@�4@���@���@���@���@�hs@��m@�>B@~M�@}�z@|��@|�9@|��@|j@|'R@{ݘ@zߤ@y��@y�@x��@w��@v1�@u�'@u:�@t�$@tC-@sy�@r�b@re@q��@qV@p�v@p�$@o��@o�4@ob�@oO@o>�@o�@n�@n��@n��@np;@n8�@m�o@m�@m�C@mIR@l�e@lM@k��@ko@jB[@i�@i�^@irG@iG�@i4@i!�@i�@i@h(�@f��@f��@f��@fR�@f�@e��@e��@eVm@e(�@d�P@d֡@d�I@dS�@dM@c�W@c�@c��@c�4@ct�@cX�@c
=@b�<@b��@bd�@bYK@bE�@be@a��@a�j@a�N@a�^@a��@a��@a�C@b	@b_�@a�@a�X@a�7@a}�@as�@aw2@`�@`��@`��@`h�@_ƨ@_Mj@^�@^�6@^Ov@]�@]�@]�@\��@\�E@\�D@[��@[��@Zں@Z�B@Z�,@Z��@Z1�@Y��@Ya�@Yq@Y	l@Y[W@Y�@X�$@XN�@Wn/@V��@V�A@V?@U�@U�7@T�@TA�@S�;@S��@S��@S+@S
=@R�y@Rc @R	@Q�'@Q�@P��@P�@P�[@P�p@P�@PM@O�g@Oy�@O8@N��@NYK@M��@M��@Me,@M;@L�9@LQ�@K�]@K��@K{J@J�c@J��@Js�@Jh
@J^5@JH�@J4@I�H@I4@H�/@H��@H?�@H�@G�&@G�@F��@FJ�@E�H@E�^@E�@E0�@D��@D�p@D��@D��@Dw�@D_@Db@C��@C�@C�V@C�@CC�@Bں@B�X@B�@Bh
@Be@A�@A�d@A��@A%@@��@?�]@?��@?�@>�@>��@>YK@=�)@=�@=��@=c@=IR@=@@<�K@<Ĝ@<�j@<�e@<�u@<��@<m�@<�@;�@;ƨ@;��@;��@;��@;��@;8@:��@:xl@:E�@:@9�9@9�N@9��@9&�@8ѷ@8�@8��@8?�@7�]@7�@7g�@7,�@6�L@6d�@5��@5��@5��@5k�@5N<@5@@4�E@4]d@4I�@3�@3J#@3Y@2��@2u%@24@1��@1�h@1rG@0�`@0�z@0��@0~(@0oi@0e�@0c�@0>B@0@/��@/��@/�6@/�@/�@.v�@-�Z@-�^@-��@-�7@-a�@-[W@-Y�@-N<@-�@,�K@,�v@,�`@,�O@,7@+��@+Mj@*�@*n�@*-@)�.@)��@)��@)�X@)o @)X@)!�@(�@(��@(_@'�a@'��@'�:@'�:@'�{@'C�@'�@&��@&��@&��@&q�@&$�@%�@%�^@%��@%��@%u�@%f�@%;@$�j@$��@$��@$r�@$bN@$6@$	�@$  @#�g@#�@@#Mj@#�@"�@"�'@"�b@"!�@!��@!��@!��@!��@!f�@!L�@!�@ �4@ 9X@ $@  �@ M@�
@��@��@��@��@�P@o�@W?@F�@9�@6z@/�@C@�@��@�<@��@��@E�@��@ϫ@@�-@��@��@�n@�S@|@�@�o@l"@[�@<�@~@��@H�@��@�@}V@p;@kQ@c @M�@=q@O@J�@��@?�@�@�@�@�W@ƨ@�0@�V@�4@j�@�@�s@�R@v�@�N@Y�@=�@q@�@�@~(@g8@�@�g@�$@/�@�@
=@�@�@�,@�@Ov@)�@�@�M@j@%F@@��@��@?�@�@�m@�a@��@dZ@K�@��@�m@�<@��@��@��@�D@�9@�@��@��@�X@�@�@�O@�I@�u@�Y@bN@9X@@�W@�g@��@�[@�V@��@�{@P�@
�@
��@
q�@
kQ@
Ov@
&�@	��@	@	��@	|@	o @	hs@	0�@	�@	@@	;@�U@|�@2�@~@G@�@�@��@>�@�@S@��@�'@��@@�@($@�@��@�"@x�@p�@X@Dg@:�@+@�P@��@�@�?@��@�9@��@��@�@��@�@z�@|�@Z@7�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A� A�A�$A��A�oA�A�FA�+A�A��A�bA�PA�	�A�A��A��A��sA��]A��/A��?A��?A��,A�F�A�o�A��A�zDA���A�C�A��MA���A�poA�!�A�}�A��JA�=qA�*0A���A�C�A�x8A�T�A��RA��A��lA��A� 'A��	A��XA�:�A��9A���A��A��{A��vA�[�A�ÖA��RA�CaA� �A���A���A��pA�v�A���A���A�7A�M�A��pA�3�A��A�e�A�^�A���A��pA��A��dA���A��6A�  A�jA��xA��QA���A��'A��pA�U�A���A�S[A��aA�{JA���A�L�A�~A�=�A�A��A���A��SA��A���A��xA��TA���A��oA��A}�TA|SA{��A{	lAz[WAy��Ax�MAw��AujAs<6Apz�An>�Am��Al�"Al  AjȴAj�Ail�Ah�MAhB�Ah*0Ah�Ag��Af��Ae�Ad �Ab�wAb�A`��A_<�A]CA\qA[��A[=AZ�FAYxlAW��AV��AUATcAR�KAPݘAPhsAP%FAO'�AN|�AM�QALMAKH�AJ�ZAIںAFoiACԕAB�A@6A>�WA>U2A=�)A=�A<_A;��A9��A7u�A6e�A6�A5kQA4%FA3kQA1�PA/��A.3�A-
�A,:�A+dZA+1�A+CA*�]A*�$A)�$A(��A('�A&��A&�A%�xA%	A$��A$c�A$5?A$A#�}A#FtA"�A!ƨA!L0A ��A -�A�dA��A�'A,=A+A �A�AMA�?A�A�A6�AݘAl"AM�AϫAXyA�]Ah�A;dA>BA��A��A�"AA��A6A
��A
��A
C-A	��A:�A�A�A{AjA�cA�wA|�A�Am]AJAc A�$A}VA&�A�`A�A �6@�Z�@�m]@�Ta@��@��:@��@�ϫ@��$@��@��5@�4@�&�@�@� �@�o�@��@�7�@�C�@�4@�`�@�kQ@���@�e�@�u@��}@�5?@�33@�
�@ݎ"@��,@�{�@ۥ�@��@؏\@��]@װ�@��p@�n�@�o@��@�~�@�@O@��@е@ϕ�@��@�u%@�=@���@Ⱦ@�l"@���@���@İ!@��@_@�@���@�8@��@�K�@�ѷ@���@�h�@�Q�@�M@�9X@��@���@��c@��@�l"@�>�@�/�@�ں@�`�@�&�@��@�F�@��K@�x@���@�e,@�q@���@��}@���@�v`@�>�@��@��A@��P@�	l@�!�@���@���@�N<@�($@�c@�G@�o@���@�oi@�H@���@���@�F�@���@��@�?}@��p@���@�d�@�U2@�Z@�W�@�A�@� �@��>@��@���@�C@��2@��b@�kQ@�]d@��)@�h�@���@���@�Q�@��@���@���@�6@��@���@���@�x�@�j�@�=�@��@�@@�S@�-@�+@��@��@�6�@�ԕ@���@��@�($@�s@���@��4@���@�m�@�Ta@�@�;@��K@���@�l�@�bN@��@���@�q@�Ta@�H�@�:*@�,=@��+@��q@��$@�+@���@�_@��.@���@�G�@���@��<@�.�@���@�_p@�-w@��)@��!@��x@�v�@�V@�C�@�=q@�2�@��@�"h@�@�@�g8@��@�v�@��@���@��@��4@���@���@�\�@�4@���@���@���@���@�hs@��m@�>B@~M�@}�z@|��@|�9@|��@|j@|'R@{ݘ@zߤ@y��@y�@x��@w��@v1�@u�'@u:�@t�$@tC-@sy�@r�b@re@q��@qV@p�v@p�$@o��@o�4@ob�@oO@o>�@o�@n�@n��@n��@np;@n8�@m�o@m�@m�C@mIR@l�e@lM@k��@ko@jB[@i�@i�^@irG@iG�@i4@i!�@i�@i@h(�@f��@f��@f��@fR�@f�@e��@e��@eVm@e(�@d�P@d֡@d�I@dS�@dM@c�W@c�@c��@c�4@ct�@cX�@c
=@b�<@b��@bd�@bYK@bE�@be@a��@a�j@a�N@a�^@a��@a��@a�C@b	@b_�@a�@a�X@a�7@a}�@as�@aw2@`�@`��@`��@`h�@_ƨ@_Mj@^�@^�6@^Ov@]�@]�@]�@\��@\�E@\�D@[��@[��@Zں@Z�B@Z�,@Z��@Z1�@Y��@Ya�@Yq@Y	l@Y[W@Y�@X�$@XN�@Wn/@V��@V�A@V?@U�@U�7@T�@TA�@S�;@S��@S��@S+@S
=@R�y@Rc @R	@Q�'@Q�@P��@P�@P�[@P�p@P�@PM@O�g@Oy�@O8@N��@NYK@M��@M��@Me,@M;@L�9@LQ�@K�]@K��@K{J@J�c@J��@Js�@Jh
@J^5@JH�@J4@I�H@I4@H�/@H��@H?�@H�@G�&@G�@F��@FJ�@E�H@E�^@E�@E0�@D��@D�p@D��@D��@Dw�@D_@Db@C��@C�@C�V@C�@CC�@Bں@B�X@B�@Bh
@Be@A�@A�d@A��@A%@@��@?�]@?��@?�@>�@>��@>YK@=�)@=�@=��@=c@=IR@=@@<�K@<Ĝ@<�j@<�e@<�u@<��@<m�@<�@;�@;ƨ@;��@;��@;��@;��@;8@:��@:xl@:E�@:@9�9@9�N@9��@9&�@8ѷ@8�@8��@8?�@7�]@7�@7g�@7,�@6�L@6d�@5��@5��@5��@5k�@5N<@5@@4�E@4]d@4I�@3�@3J#@3Y@2��@2u%@24@1��@1�h@1rG@0�`@0�z@0��@0~(@0oi@0e�@0c�@0>B@0@/��@/��@/�6@/�@/�@.v�@-�Z@-�^@-��@-�7@-a�@-[W@-Y�@-N<@-�@,�K@,�v@,�`@,�O@,7@+��@+Mj@*�@*n�@*-@)�.@)��@)��@)�X@)o @)X@)!�@(�@(��@(_@'�a@'��@'�:@'�:@'�{@'C�@'�@&��@&��@&��@&q�@&$�@%�@%�^@%��@%��@%u�@%f�@%;@$�j@$��@$��@$r�@$bN@$6@$	�@$  @#�g@#�@@#Mj@#�@"�@"�'@"�b@"!�@!��@!��@!��@!��@!f�@!L�@!�@ �4@ 9X@ $@  �@ M@�
@��@��@��@��@�P@o�@W?@F�@9�@6z@/�@C@�@��@�<@��@��@E�@��@ϫ@@�-@��@��@�n@�S@|@�@�o@l"@[�@<�@~@��@H�@��@�@}V@p;@kQ@c @M�@=q@O@J�@��@?�@�@�@�@�W@ƨ@�0@�V@�4@j�@�@�s@�R@v�@�N@Y�@=�@q@�@�@~(@g8@�@�g@�$@/�@�@
=@�@�@�,@�@Ov@)�@�@�M@j@%F@@��@��@?�@�@�m@�a@��@dZ@K�@��@�m@�<@��@��@��@�D@�9@�@��@��@�X@�@�@�O@�I@�u@�Y@bN@9X@@�W@�g@��@�[@�V@��@�{@P�@
�@
��@
q�@
kQ@
Ov@
&�@	��@	@	��@	|@	o @	hs@	0�@	�@	@@	;@�U@|�@2�@~@G@�@�@��@>�@�@S@��@�'@��@@�@($@�@��@�"@x�@p�@X@Dg@:�@+@�P@��@�@�?@��@�9@��@��@�@��@�@z�@|�@Z@7�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BGEBG+BGEBGzBG_BGEBG+BGzBGzBG�BG_BG�BG�BHBHBG�BF�BF�BF%BEmBA�B9�B�B�`B��B�MB�GB��B�CB��B�B�'B�~BٴB�:B��B�yB�B̘B�;B�XB�DBŢB��B��B��B��B��B�OB�B��B�/B�/B��B��B�"B�GB��B�tB��B�FB��B��Bt�B`�B+B(>BDB��B޸B��B��B�XB��B�+B�nB��B�*B��B�YB}<BwLBm�B^�BVBPbBD�B<PB(
B]BbB B��B�BؓB�B��B��B\)BI�B?B33B \B�B
�B
�.B
�B
�>B
��B
�B
�eB
�NB
�B
āB
�ZB
�B
�HB
�!B
�1B
�9B
�NB
��B
�NB
��B
�.B
��B
�0B
��B
��B
HB
vzB
r�B
k�B
a|B
N�B
G+B
K^B
R�B
Q B
G�B
=VB
7�B
&LB
�B
�B	��B	�	B	��B	� B	�sB	�B	�B	�B	�nB	�B	�mB	żB	�B	��B	��B	�]B	�KB	��B	�B	�-B	�B	��B	��B	��B	��B	�%B	�uB	}�B	u�B	pUB	k�B	h
B	c�B	b�B	b�B	a�B	_pB	\xB	WYB	V�B	P�B	L~B	I�B	J#B	L�B	LJB	J�B	JXB	IlB	G+B	D�B	?�B	>�B	=<B	:�B	49B	/�B	.B	,=B	-�B	*�B	)_B	($B	'�B	%`B	#�B	"�B	!�B	 vB	VB	�B	/B	�B	�B	EB	FB	�B	(B	^B		�B		�B	�B	�B	�B	�B	AB��B�]B��B�VB�0B�*B�rB��B�lB��B��B��B�B�3B��B��B�AB��B�)B�QB�6B�B��B�>B�8B��B��B�:B��BߤBߊB�B�RB�DB��B�
B�fB�B�B�B�fB��B�RB�*B��B�*B�B�$B�B�B�$B�B��B�B�B�,B�LB�tB��B� B�hB�B��B�&B�B�B��B��B�hB��B�-B�pBܒB�dB�B�B�jB��B��B��B�vB߾B��B�B�B��B�ZB�,B�B�B��B��B�,B�B��B��B�B�B�6B�B��B�B�PB	�B	)B	�B	�B	uB	B	VB	# B	$ZB	0�B	33B	4B	5tB	5�B	8RB	9�B	;0B	;�B	<jB	=�B	?B	@�B	B�B	EB	I�B	JrB	KDB	K�B	K�B	K�B	K�B	L�B	N<B	N�B	P�B	RB	S�B	V�B	YeB	\�B	^OB	iDB	nIB	o�B	p�B	q�B	shB	t�B	u�B	w�B	w�B	x�B	y�B	zB	zDB	z�B	{0B	{B	z�B	~�B	��B	��B	��B	��B	� B	��B	��B	�BB	�&B	�eB	�6B	�B	��B	�B	�/B	�B	�dB	�BB	�}B	�B	��B	ƨB	�B	ȴB	ɆB	�XB	�^B	�pB	�B	�uB	׍B	��B	خB	��B	�1B	��B	�QB	��B	�]B	�B	߾B	��B	�B	�B	�B	�yB	�B	�kB	�B	�B	�5B	�;B	�AB	�|B	�tB	��B	��B	�B	�B	�PB	��B	�"B	�.B
�B
�B
?B
�B
�B
�B
)B
dB
BB
}B
�B
�B
�B
$B
�B
yB
�B
 'B
!�B
"�B
&�B
,�B
0UB
2|B
4�B
7LB
9$B
;�B
=qB
?�B
B[B
C�B
D�B
I�B
K)B
K�B
L0B
L~B
MjB
NVB
O�B
Q�B
R�B
S�B
UgB
U�B
WsB
ZkB
]B
]�B
^B
a�B
dB
eFB
e�B
fLB
fLB
ffB
f�B
f�B
fB
gmB
iDB
j�B
kQB
k�B
k�B
l=B
lWB
l�B
m)B
m�B
m�B
m�B
n�B
oiB
o�B
pUB
p�B
q[B
qvB
q�B
r�B
tB
uB
u�B
u�B
v+B
wB
w�B
xlB
x�B
y�B
{JB
|6B
|�B
~�B
�AB
��B
�_B
��B
�7B
��B
�~B
�B
��B
��B
�4B
��B
��B
��B
��B
��B
�+B
�KB
��B
�	B
�	B
�xB
��B
��B
�bB
�HB
�-B
�-B
�hB
� B
�B
��B
��B
��B
�B
��B
�}B
�B
��B
��B
�-B
�|B
�|B
��B
�%B
�%B
�tB
��B
��B
��B
�2B
��B
�xB
��B
�<B
�wB
��B
ªB
�GB
��B
ĶB
��B
�tB
��B
�zB
��B
�7B
�rB
��B
ˬB
�B
�B
�"B
ΊB
�vB
ЗB
��B
� B
��B
��B
��B
�4B
��B
�&B
�B
ԯB
��B
�MB
�gB
ՁB
�sB
�EB
�yB
��B
�KB
ٚB
�B
�QB
چB
ڠB
��B
��B
��B
��B
��B
ݘB
�B
��B
��B
��B
�\B
�B
�B
�bB
�B
�B
��B
�TB
�tB
�tB
��B
�fB
�B
��B
�B
�$B
�XB
�B
��B
�DB
�_B
�B
��B
�0B
�B
�B
�B
�=B
�B
��B
��B
�B
�B
�)B
�B
�IB
��B
�OB
�B
��B
�B
��B
�oB
�B
�B
�B
��B
�B
�B
��B
�B
�B
�`B
��B
�B
�B
�RB
��B
�	B
�XB
��B
�B
��B
�0B
�B
�dB
��B
�PB
�"B
�<B
�VB
�wB
��B
��B
�B
�.B
�.B
�.B
��B
��B
��B
��B B OB B'B�BaB�B�BB�B�BB�B�B�B�BSB%BYBBB�B�B	B	7B	7B	�B	�B
=B
�B
�B
�BDBJB~B~B~BdBBPB�BVBpB(BvB�BHBbB}B�B�BNB�B�B�BB:BTB�B�B�B@B�B�BaBaB�BgB�B�BB9BmBmB�BsBB+B+BEB�B�B1B1BB�BeBeBB�BB�B�B�BBBQB7B�BWBqB�B�B�B�B�B�B�BxBIBdBdB~B�BOB�BVBpB�B�B B B BB 'B B!�B"hB"�B#B#B# B#TB#nB#�B#�B#�B#�B$ZB$�B$�B%,B%�B&�B&�B&�B'RB'�B'�B'�B(sB(�B)*B)�B)�B)�B)�B)�B*0B*�B*�B+B+�B,"B,=B,qB,qB,�B-)B-�B-�B.B.}B.�B.�B.�B/�B/�B/�B/�B0B/�B1'B1'B1AB1'B1B1[B2|B2�B2�B2�B2�B2�B33B3�B3�B3�B4B49B4TB4TB4TB4nB4�B5ZB5�B5�B5�B6+B6FB6�B6�B6�B7LB7fB7LB7�B7�B7�B7�B8RB8�B9$B9$B9XB9rB9�B9�B:^B:�B:�B:�B:�B;JB;�B;�B<PB<�B<�B<�B<�B=B="B="B=qB=�B=�B=�B=�B=�B>B>B>B>B>(B>]B>]B>]B>�B>�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  BGEBG+BGEBGzBG_BGEBG+BGzBGzBG�BG_BG�BG�BHBHBG�BF�BF�BF%BEmBA�B9�B�B�`B��B�MB�GB��B�CB��B�B�'B�~BٴB�:B��B�yB�B̘B�;B�XB�DBŢB��B��B��B��B��B�OB�B��B�/B�/B��B��B�"B�GB��B�tB��B�FB��B��Bt�B`�B+B(>BDB��B޸B��B��B�XB��B�+B�nB��B�*B��B�YB}<BwLBm�B^�BVBPbBD�B<PB(
B]BbB B��B�BؓB�B��B��B\)BI�B?B33B \B�B
�B
�.B
�B
�>B
��B
�B
�eB
�NB
�B
āB
�ZB
�B
�HB
�!B
�1B
�9B
�NB
��B
�NB
��B
�.B
��B
�0B
��B
��B
HB
vzB
r�B
k�B
a|B
N�B
G+B
K^B
R�B
Q B
G�B
=VB
7�B
&LB
�B
�B	��B	�	B	��B	� B	�sB	�B	�B	�B	�nB	�B	�mB	żB	�B	��B	��B	�]B	�KB	��B	�B	�-B	�B	��B	��B	��B	��B	�%B	�uB	}�B	u�B	pUB	k�B	h
B	c�B	b�B	b�B	a�B	_pB	\xB	WYB	V�B	P�B	L~B	I�B	J#B	L�B	LJB	J�B	JXB	IlB	G+B	D�B	?�B	>�B	=<B	:�B	49B	/�B	.B	,=B	-�B	*�B	)_B	($B	'�B	%`B	#�B	"�B	!�B	 vB	VB	�B	/B	�B	�B	EB	FB	�B	(B	^B		�B		�B	�B	�B	�B	�B	AB��B�]B��B�VB�0B�*B�rB��B�lB��B��B��B�B�3B��B��B�AB��B�)B�QB�6B�B��B�>B�8B��B��B�:B��BߤBߊB�B�RB�DB��B�
B�fB�B�B�B�fB��B�RB�*B��B�*B�B�$B�B�B�$B�B��B�B�B�,B�LB�tB��B� B�hB�B��B�&B�B�B��B��B�hB��B�-B�pBܒB�dB�B�B�jB��B��B��B�vB߾B��B�B�B��B�ZB�,B�B�B��B��B�,B�B��B��B�B�B�6B�B��B�B�PB	�B	)B	�B	�B	uB	B	VB	# B	$ZB	0�B	33B	4B	5tB	5�B	8RB	9�B	;0B	;�B	<jB	=�B	?B	@�B	B�B	EB	I�B	JrB	KDB	K�B	K�B	K�B	K�B	L�B	N<B	N�B	P�B	RB	S�B	V�B	YeB	\�B	^OB	iDB	nIB	o�B	p�B	q�B	shB	t�B	u�B	w�B	w�B	x�B	y�B	zB	zDB	z�B	{0B	{B	z�B	~�B	��B	��B	��B	��B	� B	��B	��B	�BB	�&B	�eB	�6B	�B	��B	�B	�/B	�B	�dB	�BB	�}B	�B	��B	ƨB	�B	ȴB	ɆB	�XB	�^B	�pB	�B	�uB	׍B	��B	خB	��B	�1B	��B	�QB	��B	�]B	�B	߾B	��B	�B	�B	�B	�yB	�B	�kB	�B	�B	�5B	�;B	�AB	�|B	�tB	��B	��B	�B	�B	�PB	��B	�"B	�.B
�B
�B
?B
�B
�B
�B
)B
dB
BB
}B
�B
�B
�B
$B
�B
yB
�B
 'B
!�B
"�B
&�B
,�B
0UB
2|B
4�B
7LB
9$B
;�B
=qB
?�B
B[B
C�B
D�B
I�B
K)B
K�B
L0B
L~B
MjB
NVB
O�B
Q�B
R�B
S�B
UgB
U�B
WsB
ZkB
]B
]�B
^B
a�B
dB
eFB
e�B
fLB
fLB
ffB
f�B
f�B
fB
gmB
iDB
j�B
kQB
k�B
k�B
l=B
lWB
l�B
m)B
m�B
m�B
m�B
n�B
oiB
o�B
pUB
p�B
q[B
qvB
q�B
r�B
tB
uB
u�B
u�B
v+B
wB
w�B
xlB
x�B
y�B
{JB
|6B
|�B
~�B
�AB
��B
�_B
��B
�7B
��B
�~B
�B
��B
��B
�4B
��B
��B
��B
��B
��B
�+B
�KB
��B
�	B
�	B
�xB
��B
��B
�bB
�HB
�-B
�-B
�hB
� B
�B
��B
��B
��B
�B
��B
�}B
�B
��B
��B
�-B
�|B
�|B
��B
�%B
�%B
�tB
��B
��B
��B
�2B
��B
�xB
��B
�<B
�wB
��B
ªB
�GB
��B
ĶB
��B
�tB
��B
�zB
��B
�7B
�rB
��B
ˬB
�B
�B
�"B
ΊB
�vB
ЗB
��B
� B
��B
��B
��B
�4B
��B
�&B
�B
ԯB
��B
�MB
�gB
ՁB
�sB
�EB
�yB
��B
�KB
ٚB
�B
�QB
چB
ڠB
��B
��B
��B
��B
��B
ݘB
�B
��B
��B
��B
�\B
�B
�B
�bB
�B
�B
��B
�TB
�tB
�tB
��B
�fB
�B
��B
�B
�$B
�XB
�B
��B
�DB
�_B
�B
��B
�0B
�B
�B
�B
�=B
�B
��B
��B
�B
�B
�)B
�B
�IB
��B
�OB
�B
��B
�B
��B
�oB
�B
�B
�B
��B
�B
�B
��B
�B
�B
�`B
��B
�B
�B
�RB
��B
�	B
�XB
��B
�B
��B
�0B
�B
�dB
��B
�PB
�"B
�<B
�VB
�wB
��B
��B
�B
�.B
�.B
�.B
��B
��B
��B
��B B OB B'B�BaB�B�BB�B�BB�B�B�B�BSB%BYBBB�B�B	B	7B	7B	�B	�B
=B
�B
�B
�BDBJB~B~B~BdBBPB�BVBpB(BvB�BHBbB}B�B�BNB�B�B�BB:BTB�B�B�B@B�B�BaBaB�BgB�B�BB9BmBmB�BsBB+B+BEB�B�B1B1BB�BeBeBB�BB�B�B�BBBQB7B�BWBqB�B�B�B�B�B�B�BxBIBdBdB~B�BOB�BVBpB�B�B B B BB 'B B!�B"hB"�B#B#B# B#TB#nB#�B#�B#�B#�B$ZB$�B$�B%,B%�B&�B&�B&�B'RB'�B'�B'�B(sB(�B)*B)�B)�B)�B)�B)�B*0B*�B*�B+B+�B,"B,=B,qB,qB,�B-)B-�B-�B.B.}B.�B.�B.�B/�B/�B/�B/�B0B/�B1'B1'B1AB1'B1B1[B2|B2�B2�B2�B2�B2�B33B3�B3�B3�B4B49B4TB4TB4TB4nB4�B5ZB5�B5�B5�B6+B6FB6�B6�B6�B7LB7fB7LB7�B7�B7�B7�B8RB8�B9$B9$B9XB9rB9�B9�B:^B:�B:�B:�B:�B;JB;�B;�B<PB<�B<�B<�B<�B=B="B="B=qB=�B=�B=�B=�B=�B>B>B>B>B>(B>]B>]B>]B>�B>�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220720094255  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220720094354  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220720094355  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220720094355                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220720184400  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220720184400  QCF$                G�O�G�O�G�O�            8000JA  ARUP                                                                        20220720095936                      G�O�G�O�G�O�                