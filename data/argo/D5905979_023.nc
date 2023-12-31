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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200619170857  20220204114412  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @؊�`� 1   @؊���M�@7�/��w�cҧ-1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8ffB@  BH  BP  BX  B`  Bh  Bp  Bx  B���B���B���B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C �C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�fC�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  D   D y�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��Dy�D��D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D1��D2y�D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DM��DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy�fD�*�D�V�D���D���D��D�W�D��)D���D�&fD�U�D���D��D��D�Q�Dڈ�D��D�&�D�`RD��D��
111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��H@�{@�{A
=A?
=A_
=A
=A��A��A��A��AυA߅A�A��BBBBB'B/B8(�B?BGBOBWB_BgBoBwB�z�B��B��B��HB��HB��HB��HB��HB�{B�{B��HB��HB��HB��B��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HC 
>C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�
C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��C��RC��RC��RC��RC��RC��RC��RC��RC��RC�C��RC��RC��C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC�C��RC��RC��RC��RC��RC��RC��RD u�D �)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D��D�)D	|)D	�)D
|)D
�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D��Du�D��D|)D�)D|)D�)D|)D�)D|)D��D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D |)D �)D!|)D!�)D"|)D"�)D#|)D#�)D$|)D$�)D%|)D%�)D&|)D&�)D'|)D'�)D(|)D(�)D)|)D)�)D*|)D*�)D+|)D+�)D,|)D,�)D-|)D-�)D.|)D.�)D/|)D/�)D0|)D0�)D1|)D1��D2u�D2�)D3|)D3�)D4|)D4�)D5|)D5�)D6|)D6�)D7|)D7�)D8|)D8�)D9|)D9�)D:|)D:�)D;|)D;�)D<|)D<�)D=|)D=�)D>|)D>�)D?|)D?�)D@|)D@�)DA|)DA�)DB|)DB�)DC|)DC�)DD|)DD�)DE|)DE�)DF|)DF�)DG|)DG�)DH|)DH�)DI|)DI�)DJ|)DJ�)DK|)DK�)DL|)DL�)DM|)DM��DN|)DN�)DO|)DO�)DP|)DP�)DQ|)DQ�)DR|)DR�)DS|)DS�)DT|)DT�)DU|)DU�)DV|)DV�)DW|)DW�)DX|)DX�)DY|)DY�)DZ|)DZ�)D[|)D[�)D\|)D\�)D]|)D]�)D^|)D^�)D_|)D_�)D`|)D`�)Da|)Da�)Db|)Db�)Dc|)Dc�)Dd|)Dd�)De|)De�)Df|)Df�)Dg|)Dg�)Dh|)Dh�)Di|)Di�)Dj|)Dj�)Dk|)Dk�)Dl|)Dl�)Dm|)Dm�)Dn|)Dn�)Do|)Do�)Dp|)Dp�)Dq|)Dq�)Dr|)Dr�)Ds|)Ds�)Dt|)Dt��Dy��D�(�D�T�D���D���D��D�U�D��=D�� D�$zD�S�D�� D�޸D��D�P Dڇ
D��(D�$�D�^fD��D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���A���A���A���A���A���A���A���A�ȴA�AԾwAԬAԟ�AԓuAԏ\A�v�A�t�A�r�A�n�A�;dAӡ�A�ĜA�=qA���A�bNA���A�+A�z�A�I�A�l�AǛ�A��A�S�A��^A�(�A�Q�A�bNA�~�A��A��A��RA� �A�hsA���A��A�G�A�-A���A��A�jA��9A�JA�v�A�|�A���A��\A�dZA�~�A���A�K�A���A�jA��A�XA��^A�XA��A���A�l�A�v�A�^5A�t�A���A�~�A�33A��`A��uA�33A�r�A��A�
=A�;dA��A��yA�^5A�  A�ZA���A��wA�~�A���A�+A���A���A�5?A��A�E�A�%A��wA�%A���A�/A�E�A�ȴA�r�A��HA�K�A�A��A�#A~bNA|(�Ay"�AxQ�Av�Au�At��Ap�/AnM�AmhsAkK�Ai�^Ah�+AgXAe|�Ac�A`z�A_%A^1'A[XAZ�AX�AV�ATQ�AR��AQ+AO&�AM�AL��AL�AK��AJ=qAG�^AE�FAC/A@��A?A>~�A=�A=7LA<��A<$�A;t�A:�A:1A9
=A8$�A7A5ƨA4��A4�A3�FA3C�A2�uA0ȴA/t�A.�uA-?}A,Q�A+ƨA*��A*5?A)��A(��A(5?A'�A&�uA%�^A$r�A#t�A#O�A"�jA"JA!;dAG�A(�A�A�
A�AdZA$�A��A�A��A�A�A�A�7A �AoA1'A�PA��A��A��AJA��A33A$�AO�A%A
�`A	��A	oAr�AdZA�jA�uAZA�mAdZA�AƨAl�A�/A�DA9XA�^A �y@���@�
=@�M�@�j@���@���@��@�"�@���@�Z@�\)@��@�l�@@�9@�R@��@�F@�{@�h@��/@���@�`B@�A�@ޟ�@݉7@ܼj@�A�@��m@�@ف@���@�
=@Չ7@�`B@�G�@�&�@���@���@�33@Ұ!@��@���@���@�V@��;@�\)@��@�I�@�dZ@�V@� �@�;d@��^@�Ĝ@���@�S�@�n�@��@�7L@��D@�1@��@�  @��m@��F@�\)@��+@��^@�%@�z�@�1@��@�"�@��+@�=q@�@�@��-@���@���@���@�I�@� �@��D@��u@�V@���@��T@��T@��h@��/@��@��H@�V@�X@�/@��@�~�@���@��@��/@��9@�bN@��P@��@��^@���@���@���@�o@�@��y@���@��T@��T@���@�7L@��/@�z�@�j@�Z@�1'@��@�  @��;@��F@��F@�+@���@��y@��@��R@���@��+@�ff@�M�@�@���@���@���@��h@�hs@�&�@�%@��`@��j@���@��D@�z�@�Z@��
@���@���@�S�@�
=@�@��!@�$�@�{@�{@��T@�X@�&�@���@��9@�r�@�Z@�A�@�1@�1@��w@���@��@�dZ@�33@���@��!@��\@�M�@�J@��@��-@�X@��@��@���@���@��j@�z�@�A�@�1'@�(�@� �@�1@���@�  @���@��m@���@��w@��w@��F@��@���@�;d@��y@�=q@�J@��@���@��@�G�@��@�%@��9@��u@��u@��u@���@���@��D@�r�@�A�@��;@���@�|�@���@�-@��-@�?}@�G�@�G�@�%@�bN@�9X@�A�@� �@�  @���@��m@��w@���@���@��@��@�t�@�C�@�"�@�
=@��@��!@�~�@��@���@��7@�X@��@���@�[W@}L�@uf�@mB�@d�@[y�@S�W@M�@G|�@?t�@9��@1e,@+j�@%��@ �P@@8�@p�@�3@�9@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A���A���A���A���A���A���A���A���A���A�ȴA�AԾwAԬAԟ�AԓuAԏ\A�v�A�t�A�r�A�n�A�;dAӡ�A�ĜA�=qA���A�bNA���A�+A�z�A�I�A�l�AǛ�A��A�S�A��^A�(�A�Q�A�bNA�~�A��A��A��RA� �A�hsA���A��A�G�A�-A���A��A�jA��9A�JA�v�A�|�A���A��\A�dZA�~�A���A�K�A���A�jA��A�XA��^A�XA��A���A�l�A�v�A�^5A�t�A���A�~�A�33A��`A��uA�33A�r�A��A�
=A�;dA��A��yA�^5A�  A�ZA���A��wA�~�A���A�+A���A���A�5?A��A�E�A�%A��wA�%A���A�/A�E�A�ȴA�r�A��HA�K�A�A��A�#A~bNA|(�Ay"�AxQ�Av�Au�At��Ap�/AnM�AmhsAkK�Ai�^Ah�+AgXAe|�Ac�A`z�A_%A^1'A[XAZ�AX�AV�ATQ�AR��AQ+AO&�AM�AL��AL�AK��AJ=qAG�^AE�FAC/A@��A?A>~�A=�A=7LA<��A<$�A;t�A:�A:1A9
=A8$�A7A5ƨA4��A4�A3�FA3C�A2�uA0ȴA/t�A.�uA-?}A,Q�A+ƨA*��A*5?A)��A(��A(5?A'�A&�uA%�^A$r�A#t�A#O�A"�jA"JA!;dAG�A(�A�A�
A�AdZA$�A��A�A��A�A�A�A�7A �AoA1'A�PA��A��A��AJA��A33A$�AO�A%A
�`A	��A	oAr�AdZA�jA�uAZA�mAdZA�AƨAl�A�/A�DA9XA�^A �y@���@�
=@�M�@�j@���@���@��@�"�@���@�Z@�\)@��@�l�@@�9@�R@��@�F@�{@�h@��/@���@�`B@�A�@ޟ�@݉7@ܼj@�A�@��m@�@ف@���@�
=@Չ7@�`B@�G�@�&�@���@���@�33@Ұ!@��@���@���@�V@��;@�\)@��@�I�@�dZ@�V@� �@�;d@��^@�Ĝ@���@�S�@�n�@��@�7L@��D@�1@��@�  @��m@��F@�\)@��+@��^@�%@�z�@�1@��@�"�@��+@�=q@�@�@��-@���@���@���@�I�@� �@��D@��u@�V@���@��T@��T@��h@��/@��@��H@�V@�X@�/@��@�~�@���@��@��/@��9@�bN@��P@��@��^@���@���@���@�o@�@��y@���@��T@��T@���@�7L@��/@�z�@�j@�Z@�1'@��@�  @��;@��F@��F@�+@���@��y@��@��R@���@��+@�ff@�M�@�@���@���@���@��h@�hs@�&�@�%@��`@��j@���@��D@�z�@�Z@��
@���@���@�S�@�
=@�@��!@�$�@�{@�{@��T@�X@�&�@���@��9@�r�@�Z@�A�@�1@�1@��w@���@��@�dZ@�33@���@��!@��\@�M�@�J@��@��-@�X@��@��@���@���@��j@�z�@�A�@�1'@�(�@� �@�1@���@�  @���@��m@���@��w@��w@��F@��@���@�;d@��y@�=q@�J@��@���@��@�G�@��@�%@��9@��u@��u@��u@���@���@��D@�r�@�A�@��;@���@�|�@���@�-@��-@�?}@�G�@�G�@�%@�bN@�9X@�A�@� �@�  @���@��m@��w@���@���@��@��@�t�@�C�@�"�@�
=@��@��!@�~�@��@���@��7@�X@��G�O�@�[W@}L�@uf�@mB�@d�@[y�@S�W@M�@G|�@?t�@9��@1e,@+j�@%��@ �P@@8�@p�@�3@�9@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBhBhBhBbBbBoBoBuB{B�B�B�B�B �B$�B%�B+B+B+B-B8RBH�B;dB_;BjBw�B�JB�{B��B�bB}�B_;BbNB� B�+B�{B��B��B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B�oB�bB�JB�7B�1B�%B�B{�B{�Bu�Bt�Bt�Br�Bo�BjBaHBK�B=qB,B�BuBVB	7B��B�B�B�TB�B��B�}B�B��B�VB�+B}�Bu�Bp�B_;BP�BC�B2-B�BoBVBB
��B
��B
�B
��B
�qB
��B
�oB
�B
q�B
gmB
[#B
D�B
�B
PB
  B	�B	�/B	��B	ɺB	B	�B	��B	�hB	� B	s�B	l�B	dZB	R�B	H�B	6FB	%�B	�B	JB	B��B�B�BB�B��BĜB�wB�dB�RB�FB�-B��B��B��B�DB�+B�B�B�B�B� B~�B}�B|�Bz�Bx�Bx�Bu�Bt�Bs�Br�Br�Bo�Bq�Bk�Bm�Bo�Bp�Bo�Bl�BjBjBjBiyBhsBgmBffBe`BdZBcTBcTBaHB`BB_;BZBYBXBT�BQ�BN�BM�BL�BK�BJ�BH�BF�BF�BD�BF�BE�BC�BB�BB�BA�BD�BB�B>wB9XB5?B5?B8RB;dB8RB8RB8RB8RB7LB7LB7LB49B6FB49B49B49B49B5?B49B49B49B6FB7LB7LB7LB8RB8RB8RB9XB9XB9XB:^B8RB9XB9XB;dB;dB>wB>wB>wB?}B@�BB�BC�BD�BM�BQ�BW
BW
BW
BVBXB]/B^5B^5B^5B^5B_;B_;B`BBbNBcTBe`BhsBiyBiyBiyBiyBiyBiyBk�BiyBgmBiyBk�Bp�Bm�Bm�Bo�Bp�Bs�Bu�Bw�Bz�B{�B}�B� B�B�B�B�B�B�B�B�1B�=B�DB�VB�\B�JB�=B�JB�oB��B��B��B��B��B��B��B��B��B�B�'B�'B�'B�'B�'B�-B�3B�9B�RB�jB�jB�jB�wB��BÖBǮB��B��B��B��B��B�B�#B�5B�;B�NB�ZB�`B�fB�sB�B�B�B�B�B��B��B��B��B��B	B	B	B	%B	DB	hB	hB	hB	oB	{B	�B	�B	�B	�B	#�B	%�B	'�B	)�B	1'B	2-B	6FB	9XB	:^B	:^B	=qB	B�B	B�B	B�B	D�B	I�B	K�B	O�B	R�B	W
B	XB	YB	[#B	[#B	^5B	`BB	aHB	bNB	cTB	e`B	ffB	gmB	jB	m�B	n�B	o�B	r�B	u�B	w�B	y�B	{�B	~�B	�B	�B	�+B	�+B	�1B	�VB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�-B	�3B	�9B	�FB	�FB	�LB	�RB	�RB	�XB	�^B	�^B	�dB	�qB	�wB	�wB	�wB	�wB	��B	��B	��B	��B	B	ÖB	ĜB	ĜB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�/B	�;B	�;B	�BB	�NB	�NB	�yB	�rB
B
pB

B
;B
(>B
0!B
8B
=VB
E�B
N�B
S�B
X�B
\�B
`vB
g8B
kkB
ncB
r�B
u�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B�B�B�B�B�B	�B	�B
�B�B�B�B�B�B�B�BB"#B"#B"#B$/B/rB?�B2�BVYBa�Bn�B�eB��B��B�~BuBVZBYmBwB~IB��B��B�B�*B�$B�*B�*B�%B�B�B�B�B�B��B��B��B��B��B��B��B�kB�XBRB}GBx(Bs
Bs
Bl�Bk�Bk�Bi�Bf�Ba�BXmBB�B4�B#1B�B
�B�B cB�B��B�BڃB�MB��B��B�;B��B��B~aBu+Bl�Bg�BVtBHB:�B)iB�B	�B�B
�RB
�/B
�
B
��B
�)B
��B
��B
��B
xPB
h�B
^�B
RpB
;�B

B
�B	�TB	��B	ԆB	�UB	�B	��B	�oB	��B	��B	w^B	kB	c�B	[�B	JTB	@B	-�B	HB	B	�B�tB�DB�B׭B�pB�@B�	B��B��B��B��B��B�fB�)B�B��B~�B{�B{�By�BxzBwtBvnBuhBtbBrVBpJBpJBm8Bl1Bk,Bj&Bj&BgBi Bb�BeBgBhBgBdBa�Ba�Ba�B`�B_�B^�B]�B\�B[�BZ�BZ�BX�BW�BV�BQ�BP�BO�BLxBIgBFTBENBDHBCBBB<B@0B>$B>$B<B>$B=B;B:B:B9B<B:B5�B0�B,�B,�B/�B2�B/�B/�B/�B/�B.�B.�B.�B+�B-�B+�B+�B+�B+�B,�B+�B+�B+�B-�B.�B.�B.�B/�B/�B/�B0�B0�B0�B1�B/�B0�B0�B2�B2�B5�B5�B5�B6�B8B:B;B<BESBIlBN�BN�BN�BM�BO�BT�BU�BU�BU�BU�BV�BV�BW�BY�BZ�B\�B_�B`�B`�B`�B`�B`�B`�BcB`�B^�B`�BcBh$BeBeBgBh$Bk6BmCBoOBraBsgBusBwBz�Bx�By�By�Bz�Bz�B|�B�B��B��B��B��B��B��B��B��B�B�<B�fB�aB�aB�gB�gB�nB�zB��B��B��B��B��B��B��B��B��B��B��B��B��B��B� B�B�+B�OB�OB�UB�[B�hB̀BҞBհBֶB��B��B��B��B��B��B��B�B�B�*B�UB�UB�`B�`B�mB�B��B��B��B	�B	�B	�B	�B		�B	�B	�B	B	B	)B	NB	ZB	gB	!sB	(�B	)�B	-�B	0�B	1�B	1�B	4�B	:B	:B	:B	<B	A/B	C<B	GSB	JfB	N~B	O�B	P�B	R�B	R�B	U�B	W�B	X�B	Y�B	Z�B	\�B	]�B	^�B	a�B	eB	fB	gB	j"B	m5B	oAB	qMB	sYB	vlB	z�B	|�B	~�B	~�B	�B	��B	��B	�
B	�B	�B	�B	�.B	�.B	�4B	�:B	�@B	�XB	�eB	�wB	�wB	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�!B	�'B	�:B	�FB	�RB	�RB	�YB	�YB	�_B	�qB	�qB	�wB	ЃB	ЃB	щB	ԛB	֧B	֧B	׮B	ٺG�O�B	��B	��B	��B
�B
tB
�B
�B
'�B
/lB
4�B
=>B
F'B
K+B
O�B
TB
W�B
^�B
b�B
e�B
jB
m*111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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