CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:00Z creation      
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
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \h   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^\   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �T   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �H   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �<   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �<   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �<   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �<   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �h   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �l   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �p   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �t   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �x   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181005190500  20181005190500  5904950 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               A   AO  6429                            2B  A   APEX                            7464                            062512                          846 @ס$����1   @ס%F<@4Q���l��c�����1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   BffBffBffB   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B���B���B�  B�33B�33B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C�C
  C  C  C  C  C  C  C�fC  C  C  C   C"  C$  C&  C(  C*  C,  C.�C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Ca�fCd  Cf  Ch  Cj  Cl  Cn�Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C��3C�  C�  C�  C��3C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D fD � D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+fD+� D,  D,�fD-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3�fD4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=y�D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DCfDC� DD  DD� DE  DE� DF  DF� DF��DG� DH  DH� DIfDI�fDJfDJ� DK  DKy�DL  DL� DM  DM� DN  DN� DO  DO� DP  DPy�DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da�fDb  Db� Dc  Dc� Dd  Dd� De  Dey�Df  Df� Dg  Dgy�Dh  Dh� Dh��Di� Di��Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dny�Dn��Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dw�3Dy��D�8�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�z�@�{@�{A
=A?
=A_
=A
=A��A��A��A��AυA߅A�A��B(�B(�B(�BB'B/B7B?BGBOBWB_BgBoBwBB��HB��B��B��HB�{B�{B��HB��HB�{B��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HC�C�C�C
>C	�C�C�C�C�C�C�C�
C�C�C�C�C!�C#�C%�C'�C)�C+�C.
>C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�
Cc�Ce�Cg�Ci�Ck�Cn
>Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��RC��C��RC��RC��RC��C��RC��RC��RC��C��RC��RC��RC��RC��RC��RC��RC��RC��RC��C��RC��RC��RC��RC��RC��RC�C�C��RC��RC��RC��RC��RC��RC��RC��RC��RC�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RD �D |)D �)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)Du�D�)D|)D�)D|)D�)D	|)D	�)D
|)D
�)D|)D�)D|)D�)D|)D�)Du�D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D |)D �)D!|)D!�)D"|)D"�)D#|)D#�)D$|)D$�)D%|)D%�)D&|)D&�)D'|)D'�)D(|)D(�)D)|)D)�)D*|)D+�D+|)D+�)D,��D,�)D-|)D-�)D.|)D.�)D/|)D/�)D0|)D0�)D1|)D1�)D2|)D2�)D3��D3�)D4|)D4�)D5|)D5�)D6|)D6�)D7|)D7�)D8|)D8�)D9|)D9�)D:|)D:�)D;|)D;�)D<|)D<�)D=u�D=�)D>|)D>�)D?|)D?�)D@|)D@�)DA|)DA�)DB|)DC�DC|)DC�)DD|)DD�)DE|)DE�)DF|)DF��DG|)DG�)DH|)DI�DI��DJ�DJ|)DJ�)DKu�DK�)DL|)DL�)DM|)DM�)DN|)DN�)DO|)DO�)DPu�DP�)DQ|)DQ�)DR|)DR�)DS|)DS�)DT|)DT�)DU|)DU�)DV|)DV�)DW|)DW�)DX|)DX�)DY|)DY�)DZ|)DZ�)D[|)D[�)D\|)D\�)D]|)D]�)D^|)D^�)D_|)D_�)D`|)D`�)Da��Da�)Db|)Db�)Dc|)Dc�)Dd|)Dd�)Deu�De�)Df|)Df�)Dgu�Dg�)Dh|)Dh��Di|)Di��Dj|)Dj�)Dk|)Dk�)Dl|)Dl�)Dm|)Dm�)Dnu�Dn��Do|)Do�)Dp|)Dp�)Dq|)Dq�)Dr|)Dr�)Ds|)Ds�)Dt|)Dt�)Du|)Du�)Dv|)Dv�)Dw|)Dw�\Dy�D�6�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AɓuAɓuAɕ�AɍPA�I�AȸRA��
A�/A�A�A� �A�;dA�
=AÃA��A���A�A¡�A¾wA���A���A�r�A�A�n�A�%A��
A��uA�/A��A���A�bA��A��A��A��A��A�S�A���A��#A��A�r�A���A���A��PA��A���A��PA��hA��uA���A�  A��HA�(�A�
=A�9XA��`A��-A��A�I�A���A�VA�ĜA��PA��9A�A��/A�bNA��#A�ȴA��A��A��DA��\A��A�z�A�JA���A��DA�S�A��
A�1'A�{A��wA��/A�l�A��\A��HA�G�A��RA���A}�^Awp�Ar��Aq�Ao��AlE�Ah�9Ag��Ac��Aa33A^�A]VA\  AZ��AYhsAV��AT�AP�/AM�AMK�AK��AJJAH�DAG`BAG�AFn�AE��AD�ADAA�PA?x�A=�A;p�A:��A:Q�A9x�A7�;A61A4M�A21'A1�A0�`A.��A-�A*�uA(�A'
=A%��A#�A"�A!�FA 9XA�wAoA �A1AXA"�A��A��A33Av�A��A�-A�A�7A��A�AA\)AI�A��Ap�AhsAC�A��AA�A�
A��AS�A%A�PA�AffAx�A
ȴA
v�A
9XA
 �A
{A	�^A	�Az�A�;AȴAl�A�HA�\A�-A�A�jAZA�@�K�@�Ĝ@��P@���@���@��@���@�-@��@�1'@���@�F@�C�@@�h@��@�%@��/@��@�w@�X@��@�V@���@�7@�%@ߝ�@ޟ�@�M�@��@ܣ�@�dZ@��@��@؋D@�K�@֗�@ָR@�C�@��H@��H@��@�
=@�;d@׶F@��/@�G�@�C�@�o@���@�~�@�t�@�+@�J@Ѻ^@��y@���@Ͳ-@̋D@��@�r�@�l�@�;d@�
=@��@Ƨ�@�v�@�^5@�E�@š�@�&�@�C�@��^@�x�@���@�G�@�%@���@���@���@���@��/@�Ĝ@��D@��m@��@���@��@�\)@��y@��@�x�@��@��@��@�@���@�|�@���@�V@��@���@���@��
@�+@��@��/@��u@�bN@�  @�S�@��R@�V@�@��@���@��@��@�A�@�S�@���@�~�@�V@��T@��7@�O�@�Ĝ@�1'@���@��@�S�@�@��H@��@��R@��\@�{@��^@���@��@�`B@�hs@�`B@�7L@�V@���@��`@���@��9@���@��D@��D@��@�z�@�z�@�r�@�I�@�1@���@��F@�t�@�33@��+@�-@���@��j@�Q�@�A�@�9X@�b@��!@�$�@��#@��^@��@�X@�?}@�G�@��@���@�Ĝ@�Ĝ@��9@���@�Z@� �@��;@���@�t�@�S�@��@�ƨ@��F@�ƨ@��@��@�C�@��@�n�@���@�&�@��j@�Q�@�1@��F@�+@���@�v�@�V@�-@�{@��T@��-@��@�G�@���@��@� �@���@��F@���@���@�|�@�dZ@�\)@�S�@�33@���@��+@���@���@�p�@�G�@��@��`@���@�Ĝ@��@�j@���@��w@��P@�t�@�\)@�K�@�C�@�;d@�@���@�E�@�@��#@��@�G�@�%@�Ĝ@��@���@��D@�b@�ƨ@��@��P@�;d@�
=@���@���@�n�@�$�@�J@��@��-@�X@�%@��`@��u@���@�j@�9X@�1@��m@��;@�ƨ@��@���@���@��@�\)@���@�ff@�5?@��h@�z�@�(�@�  @���@���@�dZ@�;d@��@���@�v�@�=q@�:�@n҉11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   AɓuAɓuAɕ�AɍPA�I�AȸRA��
A�/A�A�A� �A�;dA�
=AÃA��A���A�A¡�A¾wA���A���A�r�A�A�n�A�%A��
A��uA�/A��A���A�bA��A��A��A��A��A�S�A���A��#A��A�r�A���A���A��PA��A���A��PA��hA��uA���A�  A��HA�(�A�
=A�9XA��`A��-A��A�I�A���A�VA�ĜA��PA��9A�A��/A�bNA��#A�ȴA��A��A��DA��\A��A�z�A�JA���A��DA�S�A��
A�1'A�{A��wA��/A�l�A��\A��HA�G�A��RA���A}�^Awp�Ar��Aq�Ao��AlE�Ah�9Ag��Ac��Aa33A^�A]VA\  AZ��AYhsAV��AT�AP�/AM�AMK�AK��AJJAH�DAG`BAG�AFn�AE��AD�ADAA�PA?x�A=�A;p�A:��A:Q�A9x�A7�;A61A4M�A21'A1�A0�`A.��A-�A*�uA(�A'
=A%��A#�A"�A!�FA 9XA�wAoA �A1AXA"�A��A��A33Av�A��A�-A�A�7A��A�AA\)AI�A��Ap�AhsAC�A��AA�A�
A��AS�A%A�PA�AffAx�A
ȴA
v�A
9XA
 �A
{A	�^A	�Az�A�;AȴAl�A�HA�\A�-A�A�jAZA�@�K�@�Ĝ@��P@���@���@��@���@�-@��@�1'@���@�F@�C�@@�h@��@�%@��/@��@�w@�X@��@�V@���@�7@�%@ߝ�@ޟ�@�M�@��@ܣ�@�dZ@��@��@؋D@�K�@֗�@ָR@�C�@��H@��H@��@�
=@�;d@׶F@��/@�G�@�C�@�o@���@�~�@�t�@�+@�J@Ѻ^@��y@���@Ͳ-@̋D@��@�r�@�l�@�;d@�
=@��@Ƨ�@�v�@�^5@�E�@š�@�&�@�C�@��^@�x�@���@�G�@�%@���@���@���@���@��/@�Ĝ@��D@��m@��@���@��@�\)@��y@��@�x�@��@��@��@�@���@�|�@���@�V@��@���@���@��
@�+@��@��/@��u@�bN@�  @�S�@��R@�V@�@��@���@��@��@�A�@�S�@���@�~�@�V@��T@��7@�O�@�Ĝ@�1'@���@��@�S�@�@��H@��@��R@��\@�{@��^@���@��@�`B@�hs@�`B@�7L@�V@���@��`@���@��9@���@��D@��D@��@�z�@�z�@�r�@�I�@�1@���@��F@�t�@�33@��+@�-@���@��j@�Q�@�A�@�9X@�b@��!@�$�@��#@��^@��@�X@�?}@�G�@��@���@�Ĝ@�Ĝ@��9@���@�Z@� �@��;@���@�t�@�S�@��@�ƨ@��F@�ƨ@��@��@�C�@��@�n�@���@�&�@��j@�Q�@�1@��F@�+@���@�v�@�V@�-@�{@��T@��-@��@�G�@���@��@� �@���@��F@���@���@�|�@�dZ@�\)@�S�@�33@���@��+@���@���@�p�@�G�@��@��`@���@�Ĝ@��@�j@���@��w@��P@�t�@�\)@�K�@�C�@�;d@�@���@�E�@�@��#@��@�G�@�%@�Ĝ@��@���@��D@�b@�ƨ@��@��P@�;d@�
=@���@���@�n�@�$�@�J@��@��-@�X@�%@��`@��u@���@�j@�9X@�1@��m@��;@�ƨ@��@���@���@��@�\)@���@�ff@�5?@��h@�z�@�(�@�  @���@���@�dZ@�;d@��@���@�v�@�=q@�:�@n҉11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BI�BJ�BK�BI�Bv�B��B�+Bs�B��B�dB��B�B��BĜBȴB��B�#B�BB%�BL�Bu�B�B�B�B�=B�uB��B��B��B�dBƨB��B��B�;B�HB�`B��B+BbB{B�BB��B�wB��BɺB�
B�B��B�3B��B�3B��Be`B_;B\)BH�BA�B?}B2-BPBB%B��B�B�B��B��B��B�Bt�BaHBVB@�B�B  B
�NB
��B
ŢB
ȴB
ȴB
�3B
��B
�oB
�=B
� B
\)B
A�B
$�B	��B	�B	��B	B	�B	��B	�VB	w�B	hsB	XB	N�B	G�B	@�B	6FB	'�B	�B	1B��B��B�B�yB�B�mB�yB�mB�`B�TB�;B��B��BB�FB�?B�3B�'B�B��B��B��B��B��B��B�{B�PB�7B�DB�VB�JB�\B�PB�DB�JB�VB�hB��B��B��B��B��B�oB��B��B�B�3B�XB�RB�RB�LB�?B�B��B��B��B��B��B��B��B�B�B�B��B��B��B�wB�qB�qB�wB�wB�wB�}B��B�}B�wB�jB�dB�qB�qB�dB�^B�dB�dB�wB��B�}B�}B�}B�wB�jB�dB�wB��B��B��B��B��B��B��BÖBǮBǮBǮBȴB��B��B��B��B��B��B��B��B��B��B�B�
B��B��B��B��B��B��B�HB�fB�B�B��B��B��B	
=B	hB	PB	�B	�B	�B	$�B	)�B	+B	�B	hB	PB	JB	
=B	B��B��B	  B	B	%B	bB	oB	{B	�B	�B	!�B	�B	�B	�B	#�B	%�B	'�B	(�B	+B	-B	/B	0!B	1'B	2-B	5?B	8RB	9XB	:^B	=qB	?}B	=qB	<jB	A�B	J�B	N�B	L�B	M�B	N�B	L�B	L�B	N�B	O�B	P�B	R�B	R�B	XB	ZB	\)B	]/B	`BB	dZB	ffB	ffB	hsB	hsB	hsB	k�B	l�B	o�B	p�B	u�B	t�B	u�B	x�B	{�B	{�B	z�B	y�B	y�B	z�B	{�B	� B	�B	�B	�B	�%B	�7B	�=B	�DB	�JB	�PB	�hB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�!B	�'B	�-B	�3B	�LB	�RB	�^B	�jB	�jB	�qB	�}B	��B	��B	B	ĜB	ƨB	ǮB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�#B	�#B	�B	�
B	��B	�
B	�B	�B	�B	�B	�B	�B	�#B	�#B	�#B	�#B	�#B	�)B	�)B	�/B	�5B	�;B	�HB	�HB	�HB	�HB	�NB	�NB	�NB	�NB	�TB	�ZB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
1B
1B
1B
	7B
	7B
	7B
	7B
	7B
	7B
DB
DB
DB
DB
DB
DB
JB
VB
VB
VB
\B
bB
hB
hB
oB
oB
oB
uB
uB
uB
{B
{B
�22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   BI�BJ�BK�BI�Bv�B��B�+Bs�B��B�dB��B�B��BĜBȴB��B�#B�BB%�BL�Bu�B�B�B�B�=B�uB��B��B��B�dBƨB��B��B�;B�HB�`B��B+BbB{B�BB��B�wB��BɺB�
B�B��B�3B��B�3B��Be`B_;B\)BH�BA�B?}B2-BPBB%B��B�B�B��B��B��B�Bt�BaHBVB@�B�B  B
�NB
��B
ŢB
ȴB
ȴB
�3B
��B
�oB
�=B
� B
\)B
A�B
$�B	��B	�B	��B	B	�B	��B	�VB	w�B	hsB	XB	N�B	G�B	@�B	6FB	'�B	�B	1B��B��B�B�yB�B�mB�yB�mB�`B�TB�;B��B��BB�FB�?B�3B�'B�B��B��B��B��B��B��B�{B�PB�7B�DB�VB�JB�\B�PB�DB�JB�VB�hB��B��B��B��B��B�oB��B��B�B�3B�XB�RB�RB�LB�?B�B��B��B��B��B��B��B��B�B�B�B��B��B��B�wB�qB�qB�wB�wB�wB�}B��B�}B�wB�jB�dB�qB�qB�dB�^B�dB�dB�wB��B�}B�}B�}B�wB�jB�dB�wB��B��B��B��B��B��B��BÖBǮBǮBǮBȴB��B��B��B��B��B��B��B��B��B��B�B�
B��B��B��B��B��B��B�HB�fB�B�B��B��B��B	
=B	hB	PB	�B	�B	�B	$�B	)�B	+B	�B	hB	PB	JB	
=B	B��B��B	  B	B	%B	bB	oB	{B	�B	�B	!�B	�B	�B	�B	#�B	%�B	'�B	(�B	+B	-B	/B	0!B	1'B	2-B	5?B	8RB	9XB	:^B	=qB	?}B	=qB	<jB	A�B	J�B	N�B	L�B	M�B	N�B	L�B	L�B	N�B	O�B	P�B	R�B	R�B	XB	ZB	\)B	]/B	`BB	dZB	ffB	ffB	hsB	hsB	hsB	k�B	l�B	o�B	p�B	u�B	t�B	u�B	x�B	{�B	{�B	z�B	y�B	y�B	z�B	{�B	� B	�B	�B	�B	�%B	�7B	�=B	�DB	�JB	�PB	�hB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�!B	�'B	�-B	�3B	�LB	�RB	�^B	�jB	�jB	�qB	�}B	��B	��B	B	ĜB	ƨB	ǮB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�#B	�#B	�B	�
B	��B	�
B	�B	�B	�B	�B	�B	�B	�#B	�#B	�#B	�#B	�#B	�)B	�)B	�/B	�5B	�;B	�HB	�HB	�HB	�HB	�NB	�NB	�NB	�NB	�TB	�ZB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
1B
1B
1B
	7B
	7B
	7B
	7B
	7B
	7B
DB
DB
DB
DB
DB
DB
JB
VB
VB
VB
\B
bB
hB
hB
oB
oB
oB
uB
uB
uB
{B
{B
�22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.06 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190500                              AO  ARCAADJP                                                                    20181005190500    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190500  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190500  QCF$                G�O�G�O�G�O�8000            