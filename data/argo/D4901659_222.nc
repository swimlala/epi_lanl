CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-08-27T12:00:32Z creation      
references        (http://www.argodatamgt.org/Documentation   comment           user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
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
resolution        >�E�vQ�   
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�E�vQ�   
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
_FillValue                    Ah   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  Ch   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K`   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M`   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  UX   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]P   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _P   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gH   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  iH   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q@   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    y8   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {8   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �0   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �0   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �(   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �X   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �X   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �X   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �X   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �    HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �        �Argo profile    3.1 1.2 19500101000000  20200827120032  20230721230928  4901659 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  5382                            2C  D   NAVIS_A                         0371                            082713                          863 @�3��#O1   @�3��@<)x����c��E��1   GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                      �A   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B_��Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� DfDff111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��H@�{@�{A
=A?
=A_
=A
=A��A��A��A��AυA߅A�A��BBBBB'B/B7B?BGBOBWB_\)BgBoBwBB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HC�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RD |)D �)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D	|)D	�)D
|)D
�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D |)D �)D!|)D!�)D"|)D"�)D#|)D#�)D$|)D$�)D%|)D%�)D&|)D&�)D'|)D'�)D(|)D(�)D)|)D)�)D*|)D*�)D+|)D+�)D,|)D,�)D-|)D-�)D.|)D.�)D/|)D/�)D0|)D0�)D1|)D1�)D2|)D2�)D3|)D3�)D4|)D4�)D5|)D5�)D6|)D6�)D7|)D7�)D8|)D8�)D9|)D9�)D:|)D:�)D;|)D;�)D<|)D<�)D=|)D=�)D>|)D>�)D?|)D?�)D@|)D@�)DA|)DA�)DB|)DB�)DC|)DC�)DD|)DD�)DE|)DE�)DF|)DF�)DG|)DG�)DH|)DH�)DI|)DI�)DJ|)DJ�)DK|)DK�)DL|)DL�)DM|)DM�)DN|)DN�)DO|)DO�)DP|)DP�)DQ|)DQ�)DR|)DR�)DS|)DS�)DT|)DT�)DU|)DU�)DV|)DV�)DW|)DW�)DX|)DX�)DY|)DY�)DZ|)DZ�)D[|)D[�)D\|)D\�)D]|)D]�)D^|)D^�)D_|)D_�)D`|)D`�)Da|)Da�)Db|)Db�)Dc|)Dc�)Dd|)Dd�)De|)De�)Df|)Df�)Dg|)Dg�)Dh|)Dh�)Di|)Di�)Dj|)Dj�)Dk|)Dk�)Dl|)Dl�)Dm|)Dm�)Dn|)Dn�)Do|)Do�)Dp|)Dp�)Dq|)Dq�)Dr|)Dr�)Ds|)Ds�)Dt|)Dt�)Du|)Du�)Dv|)Dv�)Dw|)Dw�)Dx|)Dx�)Dy|)Dy�)Dz|)Dz�)D{|)D{�)D||)D|�)D}|)D}�)D~|)D�Db�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�A��HA��`AׁA��A�ffA�/A���AռjAՍPA��AԲ-A�33A�AӃA�z�A�XA��TA�E�A��A��#A��A�K�A��DA�O�A�  A��`A�33A�p�A���A���A���A���A��A�%A�|�A���A�  A�K�A�=qA�dZA�(�A��HA�n�A�n�A�-A�33A�S�A��7A���A�VA���A�
=A�z�A�7LA�ZA��
A�n�A��#A��wA�S�A��/A�/A�z�A��hA���A�r�A��yA�K�A�A�"�A��A�-A�&�A���A�&�A��mA�A�O�A��DA�1'A�bA���A�/A���A�  A��+A��A|��A{l�Az�RAzI�Ay�
Ax�HAvz�As�7Ap��Am�Ak+Aj-Ai
=Ah �Af�`Ae7LA_�A^n�A]�mA]&�A[�A[7LAZ��AZbAY�
AY�FAY�PAYx�AY%AXJAUG�AS|�AR��AR��AQƨAP�HAO�AN��AN��AM��AMoAK�-AKAJ�!AIAG��AF��AF�uAFZAF5?AF�AE�
AE�AE?}AD�DAC�AA��AAA>��A=?}A<v�A<9XA;�TA;��A;l�A;33A:��A:(�A8��A7�hA6�`A6�+A65?A5��A4��A4M�A3�mA3\)A2M�A1�A0v�A09XA0�A/�
A/p�A.�uA-��A-?}A+��A+��A+�PA*bNA)\)A)"�A)
=A(�`A((�A&��A%�#A$�A"~�A"1'A!A n�A�
A�/AJAp�A
=A��A(�AVA�#AZA"�A�A�DA �A�A��AA�A�;A�FA�A1A�7A;dA�HA��A~�A(�A?}A�`A�/Az�A
��A	l�AĜAr�A�hAz�A�wA
=A�A�+A-A�-A �!A 1'@��@�~�@���@�`B@��@��;@���@�%@�E�@�^5@�@�1'@��;@�l�@�n�@��m@���@�@�33@�n�@���@߅@���@ۥ�@�n�@ٲ-@��/@�Z@���@���@�`B@���@ӥ�@�v�@�=q@�E�@�-@�@�V@�1@υ@��H@���@�@�7L@̴9@̣�@��@�;d@ə�@�bN@�o@��@��@å�@\@�X@���@��9@�r�@��@�l�@�n�@��@��u@�9X@�  @��F@�;d@���@�V@�5?@��^@��@��9@��@���@���@��+@���@��@�Z@�1@�"�@���@�ȴ@��+@��@���@�1'@�n�@�/@���@�j@���@�
=@���@�G�@�7L@�Ĝ@���@�|�@�S�@���@���@���@�A�@��@��R@�hs@��
@�o@���@�n�@�$�@���@�x�@�%@�I�@��
@��@�|�@�t�@�o@���@�E�@���@�&�@�bN@��@��P@���@��\@�~�@���@�`B@�Ĝ@��@��@�t�@�C�@�
=@���@�-@��@��-@��@��`@���@�j@�b@���@�
=@���@�{@�hs@���@�Ĝ@��9@��@���@��@�  @���@��@��H@�ȴ@���@�M�@���@�@��-@���@��7@�O�@�%@���@�j@�9X@��
@�l�@�"�@��@���@���@���@��\@�-@��h@�/@�%@��@�z�@�j@�Z@�I�@�(�@��;@���@�|�@�l�@�dZ@�\)@�S�@�33@�@��y@���@�ff@�E�@��T@��-@���@��7@�O�@�%@��@�Q�@��@�  @�@;d@~E�@}�T@}�h@|�@|j@{��@{S�@z�H@z�!@y��@y�^@y7L@x��@x�u@x�@xA�@w�;@w�w@w��@w\)@w+@v��@v�R@v5?@u��@u�@uV@tZ@s�
@s��@r��@r-@q��@q��@qX@p��@p�9@p�u@pr�@pQ�@pb@o��@o�P@o\)@o
=@n�y@nȴ@n�+@nE�@m�@m/111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�A��HA��`AׁA��A�ffA�/A���AռjAՍPA��AԲ-A�33A�AӃA�z�A�XA��TA�E�A��A��#A��A�K�A��DA�O�A�  A��`A�33A�p�A���A���A���A���A��A�%A�|�A���A�  A�K�A�=qA�dZA�(�A��HA�n�A�n�A�-A�33A�S�A��7A���A�VA���A�
=A�z�A�7LA�ZA��
A�n�A��#A��wA�S�A��/A�/A�z�A��hA���A�r�A��yA�K�A�A�"�A��A�-A�&�A���A�&�A��mA�A�O�A��DA�1'A�bA���A�/A���A�  A��+A��A|��A{l�Az�RAzI�Ay�
Ax�HAvz�As�7Ap��Am�Ak+Aj-Ai
=Ah �Af�`Ae7LA_�A^n�A]�mA]&�A[�A[7LAZ��AZbAY�
AY�FAY�PAYx�AY%AXJAUG�AS|�AR��AR��AQƨAP�HAO�AN��AN��AM��AMoAK�-AKAJ�!AIAG��AF��AF�uAFZAF5?AF�AE�
AE�AE?}AD�DAC�AA��AAA>��A=?}A<v�A<9XA;�TA;��A;l�A;33A:��A:(�A8��A7�hA6�`A6�+A65?A5��A4��A4M�A3�mA3\)A2M�A1�A0v�A09XA0�A/�
A/p�A.�uA-��A-?}A+��A+��A+�PA*bNA)\)A)"�A)
=A(�`A((�A&��A%�#A$�A"~�A"1'A!A n�A�
A�/AJAp�A
=A��A(�AVA�#AZA"�A�A�DA �A�A��AA�A�;A�FA�A1A�7A;dA�HA��A~�A(�A?}A�`A�/Az�A
��A	l�AĜAr�A�hAz�A�wA
=A�A�+A-A�-A �!A 1'@��@�~�@���@�`B@��@��;@���@�%@�E�@�^5@�@�1'@��;@�l�@�n�@��m@���@�@�33@�n�@���@߅@���@ۥ�@�n�@ٲ-@��/@�Z@���@���@�`B@���@ӥ�@�v�@�=q@�E�@�-@�@�V@�1@υ@��H@���@�@�7L@̴9@̣�@��@�;d@ə�@�bN@�o@��@��@å�@\@�X@���@��9@�r�@��@�l�@�n�@��@��u@�9X@�  @��F@�;d@���@�V@�5?@��^@��@��9@��@���@���@��+@���@��@�Z@�1@�"�@���@�ȴ@��+@��@���@�1'@�n�@�/@���@�j@���@�
=@���@�G�@�7L@�Ĝ@���@�|�@�S�@���@���@���@�A�@��@��R@�hs@��
@�o@���@�n�@�$�@���@�x�@�%@�I�@��
@��@�|�@�t�@�o@���@�E�@���@�&�@�bN@��@��P@���@��\@�~�@���@�`B@�Ĝ@��@��@�t�@�C�@�
=@���@�-@��@��-@��@��`@���@�j@�b@���@�
=@���@�{@�hs@���@�Ĝ@��9@��@���@��@�  @���@��@��H@�ȴ@���@�M�@���@�@��-@���@��7@�O�@�%@���@�j@�9X@��
@�l�@�"�@��@���@���@���@��\@�-@��h@�/@�%@��@�z�@�j@�Z@�I�@�(�@��;@���@�|�@�l�@�dZ@�\)@�S�@�33@�@��y@���@�ff@�E�@��T@��-@���@��7@�O�@�%@��@�Q�@��@�  @�@;d@~E�@}�T@}�h@|�@|j@{��@{S�@z�H@z�!@y��@y�^@y7L@x��@x�u@x�@xA�@w�;@w�w@w��@w\)@w+@v��@v�R@v5?@u��@u�@uV@tZ@s�
@s��@r��@r-@q��@q��@qX@p��@p�9@p�u@pr�@pQ�@pb@o��@o�P@o\)@o
=@n�y@nȴ@n�+@nE�@m�@m/111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB"�B �B�B�B�B�BuBbBJB1B  B��B�B�B�B�B�B�B��B  BB��B�B��B�?B�VB~�Br�Bn�Bk�BiyBe`BaHB_;B]/BYBP�BD�B=qB8RB49B2-B1'B)�B�B
=BB��B�B�yB�HB�#B��B��BŢB�^B�?B�B��B��B�bB�7B}�Br�BcTBVBM�BC�B7LB/B'�B!�B�B
=BB
��B
��B
�sB
��B
��B
�^B
��B
�B
u�B
m�B
ffB
_;B
W
B
H�B
?}B
;dB
8RB
49B
,B
�B

=B	��B	�fB	�B	��B	ȴB	�}B	�3B	��B	�%B	� B	|�B	z�B	x�B	v�B	r�B	p�B	o�B	n�B	m�B	l�B	iyB	dZB	ZB	Q�B	O�B	M�B	J�B	F�B	B�B	@�B	?}B	;dB	6FB	1'B	.B	,B	'�B	$�B	#�B	"�B	"�B	!�B	!�B	!�B	!�B	 �B	�B	�B	�B	hB	
=B	+B	B	B	B	B	B	  B��B��B�B�B�B�B�sB�`B�TB�BB�5B�#B�B��B��B��B��B��BǮBÖB��B�qB�RB�LB�?B�'B�B�B�B��B��B��B��B�{B�VB�PB�=B�%B�B~�B{�Bx�Bw�Bu�Bs�Bo�Bk�BgmBcTB`BB^5B]/B\)BZBYBW
BVBT�BR�BQ�BP�BP�BO�BN�BM�BK�BJ�BH�BF�BC�BA�B?}B>wB<jB:^B8RB6FB49B2-B0!B/B.B-B+B(�B'�B%�B$�B#�B!�B�B�B�B�B�B�B�B�B�B�B�B{BuBoBhBhBhBhBbBoBoBoBuBuBoBoBuBuBoBoBhBhBoBoBuB�B{B�B�B�B�B{BuB�B�B�B�B�B"�B$�B%�B%�B%�B%�B%�B%�B&�B%�B%�B&�B&�B(�B)�B)�B+B+B+B+B,B/B6FB9XB;dB=qB>wB>wB@�B@�B@�B@�BA�BA�BC�BG�BJ�BJ�BK�BK�BL�BM�BM�BN�BM�BN�BO�BO�BO�BS�BVBXB\)B\)BbNBjBm�Bo�Bp�Bq�Bs�Bu�Bv�Bz�B|�B~�B~�B~�B�B�B�B�1B�DB�bB�oB�{B��B��B��B��B��B��B��B��B��B�B�B�'B�9B�FB�RB�XB�qB��BBĜBǮB��B��B��B�B�#B�)B�)B�)B�)B�/B�BB�ZB�yB�B�B�B�B�B�B��B��B��B��B��B��B	  B	B	B	%B	1B	
=B	JB	PB	PB	VB	hB	�B	�B	�B	�B	!�B	!�B	"�B	"�B	#�B	&�B	)�B	+B	,B	-B	-B	.B	/B	1'B	1'B	2-B	6FB	7LB	;dB	=qB	=qB	>wB	@�B	C�B	I�B	J�B	L�B	M�B	O�B	Q�B	W
B	YB	[#B	^5B	`BB	cTB	ffB	iyB	k�B	n�B	p�B	r�B	t�B	t�B	u�B	v�B	x�B	x�B	y�B	z�B	{�B	|�B	~�B	�B	�B	�B	�%B	�=B	�JB	�PB	�bB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�B�B�B�B{B\BPB
=B%BB��B�B�B�fB�TB�B�B�B��B  B1B��B��B��B�dB�DB~�BjBiyBgmBffB`BB[#BYBYBW
BQ�B?}B8RB33B.B-B-B(�B�BB��B��B�B�ZB�)B�B��BɺB��B�9B�B��B��B�hB�DB�Bx�Bn�B^5BO�BH�B>wB2-B)�B"�B�B�BB
��B
��B
�B
�fB
ɺB
�jB
�LB
��B
z�B
p�B
hsB
aHB
[#B
S�B
C�B
9XB
5?B
2-B
/B
(�B
�B
+B	��B	�HB	��B	��B	ÖB	�dB	�!B	��B	~�B	y�B	w�B	u�B	r�B	p�B	l�B	jB	iyB	hsB	gmB	gmB	dZB	aHB	T�B	K�B	I�B	H�B	E�B	A�B	<jB	:^B	:^B	5?B	1'B	+B	'�B	'�B	"�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	{B	bB	VB	B	B��B��B��B��B��B��B��B��B�B�yB�fB�ZB�NB�;B�/B�B�B�B��B��BȴBǮBŢBĜBB�qB�dB�RB�'B�'B�!B�B��B��B��B��B��B��B��B�\B�1B�1B�%B� B}�By�Bu�Br�Bq�Bp�Bn�BjBgmBbNB_;BZBXBW
BVBVBQ�BP�BP�BO�BL�BK�BJ�BJ�BI�BH�BH�BE�BD�BC�BC�B>wB;dB9XB9XB7LB5?B33B1'B0!B,B)�B)�B'�B&�B%�B$�B"�B�B�B�B�B�B�B�B�B�B{B{BuBhB\B\BVBVBPBJBJBDBDB
=BJBPBJBPBPBPBJBJBPBJBJBDBDBJBJBPBVBVB\B\B\B\B\BPBbBoB�B�B�B�B�B�B�B�B�B�B�B �B�B�B �B �B"�B#�B#�B$�B$�B$�B$�B%�B(�B0!B33B5?B7LB8RB8RB:^B:^B:^B:^B;dB<jB>wBA�BD�BD�BE�BE�BF�BG�BG�BH�BG�BH�BI�BI�BJ�BM�BO�BR�BVBW
B]/BdZBgmBiyBjBk�Bm�Bo�Bp�Bt�Bv�Bx�Bx�Bx�Bz�B|�B~�B�B�B�=B�JB�VB�hB�uB�{B��B��B��B��B��B��B��B��B�B�B�!B�-B�3B�LB�^B�jB�wB��BŢBȴB��B��B��B�B�B�B�B�
B�B�5B�TB�`B�`B�fB�yB�B�B�B�B�B�B�B��B��B��B��B	  B	B	B	%B	+B	+B	1B	DB	bB	uB	�B	�B	�B	�B	�B	�B	�B	 �B	#�B	$�B	%�B	&�B	&�B	'�B	(�B	+B	+B	,B	0!B	1'B	5?B	7LB	7LB	8RB	:^B	=qB	C�B	D�B	F�B	G�B	I�B	K�B	P�B	R�B	T�B	XB	ZB	]/B	`BB	cTB	e`B	hsB	jB	l�B	n�B	n�B	o�B	p�B	r�B	r�B	s�B	t�B	u�B	v�B	x�B	z�B	{�B	}�B	� B	�B	�%B	�+B	�=B	�JB	�PB	�\B	�bB	�hB	�oB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  =m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=q��=m�h=m�h=m�h=q��=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`BPRES            TEMP            PSAL            PRES_ADJUSTED = PRES - (REPORTED_SURFACE_PRESSURE) for PADJ                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ corrects CTD thermal lag (CTL) viz. Johnson et al, 2007, JAOT, effects of pressure adjustments, and PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r.                  PADJ REPORTED_SURFACE_PRESSURE =0.06 dbar                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CTL alpha = 0.021 & tau = 21 s with error equal to |correction| and for OW r = 0.9998 (+/-0.0014), vertically averaged dS = -0.006 (+/-0.055)                                                                                                                   Pressures adjusted using reported surface pressure when warranted.                                                                                                                                                                                              none                                                                                                                                                                                                                                                            After pressure and cell thermal lag correction of salinity values, OW correction estimated using mapping scales of 8 & 4 long. and 4 & 2 lat., no PV constraint, and decorrelation time scale of 10 years.                                                      202307212300282023072123002820230721230028  AO  ARCAADJP                                                                    20200827120032    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200827120032  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200827120032  QCF$                G�O�G�O�G�O�0               PM  ARSQPADJV1.1                                                                20230712105803  QC  PRES            @���DffG�O�                PM  ARSQCTM V1.1                                                                20230712105803  QC  PSAL            @���DffG�O�                PM  ARSQCOWGV1.1CTD_2021v2 + Argo_2021v03                                       20230721230928  IP                  G�O�G�O�G�O�                