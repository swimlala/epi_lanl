CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:08:57Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         ZPRIMARY | https://orcid.org/0000-0001-7324-3159 | Matthew Alkire, University of Washington        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7(   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7,   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    70   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7@   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7P   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7`   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7h   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8    DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8$   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8D   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8H   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8L   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8l   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �T   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �d   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �h   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �x   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �|   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200619170857  20220204114412  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @؋F7�1   @؋�/vl@7�V�u�c����F1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @���@�  A   A   A>ffA^ffA�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  BxffB�  B�  B���B�  B�  B�  B�  B�33B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.�C0�C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj�Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D fD � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2fD2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY�fDZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� DgfDg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy��D�
D�Q�D��
D��HD�)D�d{D��RD��\D��D�W
D���D���D�=D�S�Dڝ�D��\D�3D�d{D��D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@�{@�{A
=A=p�A]p�A
=A��A��A��A��AυA߅A�A��BBBBB'B/B7B?BGBOBWB_BgBoBx(�BB��HB��B��HB��HB��HB��HB�{B��B��B��HB��HB��HB��HB��HB��HB��HB��HB��HB�{B��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HC�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C.
>C0
>C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Cj
>Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RD �D |)D �)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D	|)D	�)D
|)D
�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D |)D �)D!|)D!�)D"|)D"�)D#|)D#�)D$|)D$�)D%|)D%�)D&|)D&�)D'|)D'�)D(|)D(�)D)|)D)�)D*|)D*�)D+|)D+�)D,|)D,�)D-|)D-�)D.|)D.�)D/|)D/�)D0|)D0�)D1|)D2�D2|)D2�)D3|)D3�)D4|)D4�)D5|)D5�)D6|)D6�)D7|)D7�)D8|)D8�)D9|)D9�)D:|)D:�)D;|)D;�)D<|)D<�)D=|)D=�)D>|)D>�)D?|)D?�)D@|)D@�)DA|)DA�)DB|)DB�)DC|)DC�)DD|)DD�)DE|)DE�)DF|)DF�)DG|)DG�)DH|)DH�)DI|)DI�)DJ|)DJ�)DK|)DK�)DL|)DL�)DM|)DM�)DN|)DN�)DO|)DO�)DP|)DP�)DQ|)DQ�)DR|)DR�)DS|)DS�)DT|)DT�)DU|)DU�)DV|)DV�)DW|)DW�)DX|)DX�)DY��DY�)DZ|)DZ�)D[|)D[�)D\|)D\�)D]|)D]�)D^|)D^�)D_|)D_�)D`|)D`�)Da|)Da�)Db|)Db�)Dc|)Dc�)Dd|)Dd�)De|)De�)Df|)Dg�Dg|)Dg�)Dh|)Dh�)Di|)Di�)Dj|)Dj�)Dk|)Dk�)Dl|)Dl�)Dm|)Dm�)Dn|)Dn�)Do|)Do�)Dp|)Dp�)Dq|)Dq�)Dr|)Dr�)Ds|)Ds�)Dt|)Dt��Dy�D�D�P D��D��\D�=D�b�D��fD��pD��D�UD���D���D�QD�Q�Dڛ�D��pD�GD�b�D��D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�$�A�$�A�"�A�&�A�(�A�(�A�(�A�(�A�&�A��A�$�A�(�A�+A�-A�/A�/A�-A�$�A� �A� �A��A�{A�A�A�  A��A��A��A��yA��mA���A�~�AЗ�A���A��A�1'A��mA�"�A�C�A�A��A�r�A�I�A�/A��yA��mA�+A�S�A��
A���A�r�A�VA��A�%A�jA���A�%A�ƨA��wA�A�A�x�A���A�M�A��A�Q�A���A�E�A���A�=qA��!A�S�A��^A���A�v�A���A��A��A�K�A�|�A�n�A��^A�bA���A�bNA��A���A���A���A�%A�hsA��uA��^A��HA��A���A�O�A��yA�9XA�bA�t�A�JA��A��;A��-A�p�A��hA|�!A{7LAy��AxffAwdZAul�As;dApr�Ak�#Aj^5AiƨAgdZAc��AbA�AaS�A_�A]`BA[�A[33AZbAY&�AX�DAW�AV�+AT{AQAOƨAOAM��AM;dAL  AK�hAI�AG�7AF��AEABr�A@ĜA?��A>�9A<�/A;?}A8(�A6�uA4jA3O�A2��A0�A/`BA.^5A,�A+�7A*(�A)A(��A( �A'
=A&A$v�A#��A#dZA"�yA"��A"M�A"  A!+A�A^5A �AbA�;A��AXA��A�wA��A�`A�A7LA�!AM�A`BA�!AbAoAVA�
A��A�AbNA��AO�A�`A�Ax�A��A
Q�A	XA^5A��A�uA��AbA��A�yA5?A�^A��A  A?}A �+@��w@�33@�5?@��/@��D@�b@��@�=q@�O�@��@��w@��!@��T@��/@�A�@���@�+@�7@��`@�z�@�  @��H@���@�G�@�@��m@�
=@��@�@�&�@�+@��@�h@�A�@�ƨ@�v�@���@�b@�5?@١�@�x�@���@��@֟�@ԃ@Ӯ@�t�@��@�/@�|�@��@Ͳ-@�Ĝ@��@���@�Q�@�V@š�@�&�@�Z@��@�|�@���@�@��@��7@�p�@��u@��@���@��\@�V@���@��@�K�@���@�@�z�@�A�@�t�@�"�@���@�Q�@��@�
=@�v�@���@�K�@�l�@�l�@���@�v�@��h@���@�+@�"�@��9@�1@�dZ@�33@���@���@�E�@��#@��-@��^@���@�@�-@�J@���@�J@�J@��@��9@�V@��^@�x�@�?}@���@�j@���@��@��@�C�@��@��H@���@�ȴ@��R@��\@�$�@���@�@�7L@��9@�r�@���@���@�S�@���@���@���@���@�^5@�V@�M�@�E�@��@���@��@��@��@��T@��h@�G�@��@��@��u@�Q�@� �@�1@���@���@���@��@�dZ@�;d@��@���@��@��@��@�
=@��!@�n�@�V@��#@���@�O�@�7L@��@�V@��@��9@�I�@��@���@��P@�C�@�33@�"�@�"�@�o@���@�M�@��@�{@�J@�J@�{@�{@�@�@��@���@���@�X@�V@��@�?}@��@���@���@�(�@��;@��@�l�@�\)@�S�@�C�@��y@��\@�^5@�{@��h@�%@���@���@��@��
@��@�v�@�J@���@���@�`B@�?}@��@���@�j@�A�@��@���@��F@��@�33@�o@��y@��!@���@�v�@�E�@�-@�{@�@���@���@���@���@��@�G�@���@��j@��@�A�@�  @��@�S�@���@��R@���@�@O@{s@u+�@l��@aY�@Y�.@Q^�@J�@D�@>�c@:_@4M@/�*@)�7@#�@��@�f@!@Q�@�@	��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�$�A�$�A�"�A�&�A�(�A�(�A�(�A�(�A�&�A��A�$�A�(�A�+A�-A�/A�/A�-A�$�A� �A� �A��A�{A�A�A�  A��A��A��A��yA��mA���A�~�AЗ�A���A��A�1'A��mA�"�A�C�A�A��A�r�A�I�A�/A��yA��mA�+A�S�A��
A���A�r�A�VA��A�%A�jA���A�%A�ƨA��wA�A�A�x�A���A�M�A��A�Q�A���A�E�A���A�=qA��!A�S�A��^A���A�v�A���A��A��A�K�A�|�A�n�A��^A�bA���A�bNA��A���A���A���A�%A�hsA��uA��^A��HA��A���A�O�A��yA�9XA�bA�t�A�JA��A��;A��-A�p�A��hA|�!A{7LAy��AxffAwdZAul�As;dApr�Ak�#Aj^5AiƨAgdZAc��AbA�AaS�A_�A]`BA[�A[33AZbAY&�AX�DAW�AV�+AT{AQAOƨAOAM��AM;dAL  AK�hAI�AG�7AF��AEABr�A@ĜA?��A>�9A<�/A;?}A8(�A6�uA4jA3O�A2��A0�A/`BA.^5A,�A+�7A*(�A)A(��A( �A'
=A&A$v�A#��A#dZA"�yA"��A"M�A"  A!+A�A^5A �AbA�;A��AXA��A�wA��A�`A�A7LA�!AM�A`BA�!AbAoAVA�
A��A�AbNA��AO�A�`A�Ax�A��A
Q�A	XA^5A��A�uA��AbA��A�yA5?A�^A��A  A?}A �+@��w@�33@�5?@��/@��D@�b@��@�=q@�O�@��@��w@��!@��T@��/@�A�@���@�+@�7@��`@�z�@�  @��H@���@�G�@�@��m@�
=@��@�@�&�@�+@��@�h@�A�@�ƨ@�v�@���@�b@�5?@١�@�x�@���@��@֟�@ԃ@Ӯ@�t�@��@�/@�|�@��@Ͳ-@�Ĝ@��@���@�Q�@�V@š�@�&�@�Z@��@�|�@���@�@��@��7@�p�@��u@��@���@��\@�V@���@��@�K�@���@�@�z�@�A�@�t�@�"�@���@�Q�@��@�
=@�v�@���@�K�@�l�@�l�@���@�v�@��h@���@�+@�"�@��9@�1@�dZ@�33@���@���@�E�@��#@��-@��^@���@�@�-@�J@���@�J@�J@��@��9@�V@��^@�x�@�?}@���@�j@���@��@��@�C�@��@��H@���@�ȴ@��R@��\@�$�@���@�@�7L@��9@�r�@���@���@�S�@���@���@���@���@�^5@�V@�M�@�E�@��@���@��@��@��@��T@��h@�G�@��@��@��u@�Q�@� �@�1@���@���@���@��@�dZ@�;d@��@���@��@��@��@�
=@��!@�n�@�V@��#@���@�O�@�7L@��@�V@��@��9@�I�@��@���@��P@�C�@�33@�"�@�"�@�o@���@�M�@��@�{@�J@�J@�{@�{@�@�@��@���@���@�X@�V@��@�?}@��@���@���@�(�@��;@��@�l�@�\)@�S�@�C�@��y@��\@�^5@�{@��h@�%@���@���@��@��
@��@�v�@�J@���@���@�`B@�?}@��@���@�j@�A�@��@���@��F@��@�33@�o@��y@��!@���@�v�@�E�@�-@�{@�@���@���@���@���@��@�G�@���@��j@��@�A�@�  @��@�S�@���@��RG�O�@�@O@{s@u+�@l��@aY�@Y�.@Q^�@J�@D�@>�c@:_@4M@/�*@)�7@#�@��@�f@!@Q�@�@	��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�5B.BM�BbNB� B�+B�bB��B��B�B�FB�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�hB�7B�B� B|�Bz�Bl�BgmBaHB\)BT�BM�BF�B?}B33B-B �B�BuBJBB�B�B�HB��B�^B�FB�B��B� Br�BiyB[#BA�B33B#�B�B�BVB
��B
�/B
ŢB
��B
�B
t�B
`BB
L�B
)�B
1B	��B	�B	�HB	�
B	ɺB	�FB	��B	�+B	u�B	o�B	cTB	K�B	=qB	7LB	/B	$�B	�B	{B	VB	+B	B��B��B�B�NB�B��B��BɺBĜB��B�^B�B�B��B��B�hB�DB�+B�B� B{�Bv�Bu�Br�Bp�Bp�Bk�BhsBk�BiyBk�BgmBhsBhsBiyBffBgmBcTBdZBbNBbNB`BB_;B^5B^5B[#BYBYBYBXBXBXBT�BS�BT�BR�BQ�BO�BO�BM�BL�BI�BI�BF�BE�BC�BC�B@�B?}B>wB=qB<jB:^B8RB6FB5?B5?B6FB5?B33B=qB>wB@�BA�B@�B@�B>wB>wB:^B5?B7LB?}B?}B>wB>wB=qB<jB=qB=qB=qB=qB<jB<jB;dB;dB<jB<jB;dB;dB:^B;dB;dB<jB<jB<jB=qB=qB=qB=qBB�BC�BC�BC�BC�BD�BH�BI�BL�BM�BM�BQ�B\)B_;BaHBaHBaHBaHBbNBaHBaHB`BB^5B^5BbNBbNBcTBe`BffBiyBjBn�Bo�Bn�Bq�Bq�Bs�Bw�Bw�B|�B{�B{�B�B�B�B�B�B�B�B�+B�=B�DB�=B�7B�=B�DB�\B��B��B��B��B��B�B�B�B�-B�B�B�B�B�B�B�B�-B�3B�9B�FB�LB�jB�}BÖBƨBɺBɺBǮBĜBÖBBB��BBȴB��B��B��B��B��B��B��B�B�B�/B�BB�NB�sB�B�B�B��B��B��B��B��B	  B	B	B	%B	
=B	bB	oB	oB	oB	uB	uB	�B	�B	�B	#�B	$�B	%�B	(�B	-B	0!B	33B	33B	49B	6FB	9XB	:^B	;dB	<jB	=qB	?}B	A�B	E�B	F�B	F�B	F�B	F�B	H�B	K�B	L�B	M�B	N�B	R�B	ZB	^5B	^5B	`BB	aHB	aHB	bNB	e`B	iyB	k�B	k�B	o�B	t�B	v�B	x�B	z�B	|�B	�B	�B	�+B	�7B	�JB	�VB	�bB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�3B	�?B	�FB	�LB	�XB	�^B	�}B	ÖB	ƨB	ǮB	ȴB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�#B	�)B	�/B	�/B	�/B	�/B	�5B	�5B	�5B	�;B	�HB	�TB	�ZB	�`B	�`B	�fB	�mB	�sB	�yB	�yB	��B	��B
B
�B
�B
!bB
+�B
3hB
8�B
>]B
A�B
JrB
P}B
UgB
Y1B
^�B
bhB
gB
kQB
pUB
t111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�$B�*B�0B�0B�<B�`B%:BD�BYqBw"B~MB��B��B��B�;B�fB�5B�B��B�B� B��B��B��B��B��B��B��B��B��B��B��B��B��B�^Bz:Bw(BtBr	Bc�B^�BXrBSTBL)BD�B=�B6�B*aB$=B�B�B
�B{B�KB��B�B�}B�B��B�~B�SB��Bw<Bi�B`�BRbB8�B*uBB�B�B�B
�B
�wB
��B
�B
xUB
lB
W�B
D!B
!RB	��B	�)B	��B	ؤB	�gB	�B	��B	�>B	~�B	m'B	gB	Z�B	C/B	4�B	.�B	&�B	HB	B	�B	�B��B��B�\B�8B� BپB�uB�KB�EB�,B�B��B��B��B�wB�YB�B��B��B~�Bz�BwyBsaBnCBm>Bj+BhBhBcB_�BcB`�BcB^�B_�B_�B`�B]�B^�BZ�B[�BY�BY�BW�BV�BU�BU�BR�BP�BP�BP�BO�BO�BO�BL~BKxBL~BJrBIlBG_BG_BETBDNBA;BA;B>)B=$B;B;B8B6�B5�B4�B3�B1�B/�B-�B,�B,�B-�B,�B*�B4�B5�B8B9B8B8B5�B5�B1�B,�B.�B7B7B5�B5�B4�B3�B4�B4�B4�B4�B3�B3�B2�B2�B3�B3�B2�B2�B1�B2�B2�B3�B3�B3�B4�B4�B4�B4�B:B;B;B;B;B<"B@:BA@BDSBEYBEYBIrBS�BV�BX�BX�BX�BX�BY�BX�BX�BW�BU�BU�BY�BY�BZ�B\�B]�B`�BbBfBg#BfBi/Bi/Bk;BoTBoTBtsBslBslBx�Bz�B{�Bz�B{�By�B|�B~�B��B��B��B��B��B��B��B�/B�HB�TB�lB�~B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�)B�;B�;B�/B�B�B�B�B�B�B�6B�CB�CB�ZB�sB�yB�yB�BͅBϑBԯB��B��B��B�B�)B�5B�;B�NB�`B�fB�sB�~B��B��B��B	�B	�B		�B		�B		�B	
�B	
�B	B	B	;B	SB	YB	_B	 rB	$�B	'�B	*�B	*�B	+�B	-�B	0�B	1�B	2�B	3�B	4�B	6�B	9B	=B	>"B	>"B	>"B	>"B	@.B	CAB	DGB	EMB	FSB	JkB	Q�B	U�B	U�B	W�B	X�B	X�B	Y�B	\�B	`�B	b�B	b�B	gB	l3B	n@B	pLB	rXB	teB	x}B	z�B	~�B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�3B	�FB	�LB	�WB	�]B	�pB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�B	�!B	�'B	�'B	�'B	�4B	�4B	�:B	�EB	�QB	�QB	�^B	�jB	�vB	�|B	ςB	ЉB	яB	ҕB	ӛB	ԡB	ԡB	ԡB	ԡB	զB	զB	զB	֬B	عB	��B	��B	��B	��B	��B	��B	��B	��G�O�B	�]B	�B	��B
bB
WB
�B
#\B
*�B
/�B
5�B
9B
A�B
G�B
L�B
P�B
V@B
Y�B
^�B
b�B
g�B
kq111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.06 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.008(+/-0.002) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144122022020411441220220204114412  AO  ARCAADJP                                                                    20200619170857    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170857  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170857  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114412  IP                  G�O�G�O�G�O�                