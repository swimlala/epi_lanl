CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:05:09Z AOML 3.0 creation; 2016-05-31T19:14:25Z UW 3.1 conversion     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7    PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7X   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
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
resolution        :�o     �  U|   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _p   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gd   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  id   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qX   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yL   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {L   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �@   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �4   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �d   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �d   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �d   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �d   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20140721230509  20160531121425  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  4051_7090_007                   2C  D   APEX                            5368                            041511                          846 @�Ck����1   @�Cl�D�@3���vȴ�d2n��O�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A���B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	�fD
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�3Dy��D���D�C3D���D��fD��D�9�D���D���D�  D�33D�� D��fD�3D�33Dڐ D�� D�fD�6fD�p 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@�{@�{A
=A?
=A_
=A
=A��A��A��A��AυA߅A�Q�A��BBBBB'B/B7B?BGBOBWB_BgBoBwBB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HC�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RD |)D �)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D	��D	�)D
|)D
�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D |)D �)D!|)D!�)D"|)D"�)D#|)D#�)D$|)D$�)D%|)D%�)D&|)D&�)D'|)D'�)D(|)D(�)D)|)D)�)D*|)D*�)D+|)D+�)D,|)D,�)D-|)D-�)D.|)D.�)D/|)D/�)D0|)D0�)D1|)D1�)D2|)D2�)D3|)D3�)D4|)D4�)D5|)D5�)D6|)D6�)D7|)D7�)D8|)D8�)D9|)D9�)D:|)D:�)D;|)D;�)D<|)D<�)D=|)D=�)D>|)D>�)D?|)D?�)D@|)D@�)DA|)DA�)DB|)DB�)DC|)DC�)DD|)DD�)DE|)DE�)DF|)DF�)DG|)DG�)DH|)DH�)DI|)DI�)DJ|)DJ�)DK|)DK�)DL|)DL�)DM|)DM�)DN|)DN�)DO|)DO�)DP|)DP�)DQ|)DQ�)DR|)DR�)DS|)DS�)DT|)DT�)DU|)DU�)DV|)DV�)DW|)DW�)DX|)DX�)DY|)DY�)DZ|)DZ�)D[|)D[�)D\|)D\�)D]|)D]�)D^|)D^�)D_|)D_�)D`|)D`�)Da|)Da�)Db|)Db�)Dc|)Dc�)Dd|)Dd�)De|)De�)Df|)Df�)Dg|)Dg�)Dh|)Dh�)Di|)Di�)Dj|)Dj�)Dk|)Dk�)Dl|)Dl�)Dm|)Dm�)Dn|)Dn�)Do|)Do�)Dp|)Dp�)Dq|)Dq�)Dr|)Dr�)Ds|)Ds�)Dt|)Dt�\Dy��D���D�AGD���D��zD��D�7�D���D���D��D�1GD��D��zD�GD�1GDڎD��D�zD�4zD�n11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��yA��A��A��A��A��A��A��A���A���A���A��A��A���A���A��#AǙ�A�O�A�(�A�&�A�-A�"�A�$�AƝ�A�7LA�ĜAŃA�z�AœuA�&�A�p�A��A��
A���A×�A�E�A�r�AìA���A��A�G�A�9XA�Q�A�dZA�Q�A�\)A�K�A�7LA�JA��yA���A�`BA�&�A��A�ƨA���A��A�x�A�oA��PA�A�`BA�{A��A���A�A�A�%A���A���A�^5A���A�S�A�A��9A���A��7A��-A�p�A�^5A���A���A�`BA�ffA�7LA�p�A���A�XA�A�7LA�bNA�A�A���A�"�A���A��uA�"�A��A��uA���A�  A�+A�XA�ȴA�33A�ƨA�hsA�A�S�A�33A���A�Q�A��
A���A�p�A� �A��A��9A���A��mA���A�(�A��A�/A�r�A���A��A�oA�|�A�M�A�;A}G�A{dZAyƨAuAq&�Ap�HAq"�Ap�!Ao33Am+AljAk�;Ai"�Af9XAb�9A` �A]ƨA\�9A[%AYƨAXbNAUdZAS&�ARA�AQ
=AO�AM/AJbNAH��AG�AF�+AE��AEO�AD�\ACoABI�AA�7A?�hA=��A<��A:9XA8I�A6A�A5VA4E�A21A0�jA0bNA.��A-p�A-&�A,�A,�jA+\)A)��A(M�A%��A$ZA#�A!�
A ZA7LA{AK�A1'A��A�AI�A��A�yA��AdZAffA7LA1'AĜA�A�yA��A�HAM�A;dA�!AbAdZA
��A
A�A��A/A��A�A�A�wA 5?@��@���@�`B@���@�r�@��@��H@��@�1'@�r�@��P@�G�@�@��@�n�@�5?@�G�@��@�;d@�D@땁@�&�@�  @�o@�-@�z�@�O�@�9X@߅@ޟ�@��@��@��H@ڏ\@��#@ش9@ו�@���@�$�@�x�@���@�z�@�Z@���@ӕ�@��y@��@��;@�;d@θR@͙�@̬@�1'@ˍP@ʧ�@�r�@�33@�=q@���@î@��H@�@�b@��@��@�G�@���@�j@���@�\)@���@�{@��@�x�@�Ĝ@��D@��@��y@�l�@��@�@�`B@��`@���@��m@�S�@�ȴ@�=q@��7@��D@�(�@�ƨ@�C�@�
=@��y@���@�ff@���@���@�%@��D@�I�@�9X@�(�@��F@�ȴ@�=q@��@��@��-@�hs@�?}@��@�bN@�b@���@���@�"�@���@�J@�@�^5@�ff@�-@��-@��h@�hs@�?}@��D@�1@�ƨ@��@��P@���@��@���@��R@��\@�{@�@���@���@��^@�V@�I�@�b@��m@���@���@��@��H@�
=@�n�@�^5@��T@�p�@�?}@�&�@���@��@��/@�Ĝ@��@�Z@� �@�ƨ@�|�@�;d@���@�ȴ@�~�@�M�@�-@��@�@�@���@���@��h@��@�hs@�O�@�/@�V@��j@�r�@�Q�@�1'@��m@��@��@�l�@�\)@�S�@�K�@�K�@�;d@�o@��H@���@�-@���@�`B@�7L@���@��j@�j@�1'@���@���@���@�t�@�33@��@�o@�o@���@��y@��@��R@�V@�5?@��@���@�G�@���@���@�1'@�  @��;@��@�dZ@�o@��y@��R@���@���@���@��!@��R@��!@�n�@�-@�{@���@��#@���@�`B@�O�@�&�@���@�A�@��
@���@���@���@�dZ@�+@���@�{@��#@��h@�hs@�O�@�7L@�@�l�@}@o�;@hQ�@a��@Yx�@P��@J^5@B^5@<1@4��@.$�@'K�@#C�@�@��@��@�@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��yA��A��A��A��A��A��A��A���A���A���A��A��A���A���A��#AǙ�A�O�A�(�A�&�A�-A�"�A�$�AƝ�A�7LA�ĜAŃA�z�AœuA�&�A�p�A��A��
A���A×�A�E�A�r�AìA���A��A�G�A�9XA�Q�A�dZA�Q�A�\)A�K�A�7LA�JA��yA���A�`BA�&�A��A�ƨA���A��A�x�A�oA��PA�A�`BA�{A��A���A�A�A�%A���A���A�^5A���A�S�A�A��9A���A��7A��-A�p�A�^5A���A���A�`BA�ffA�7LA�p�A���A�XA�A�7LA�bNA�A�A���A�"�A���A��uA�"�A��A��uA���A�  A�+A�XA�ȴA�33A�ƨA�hsA�A�S�A�33A���A�Q�A��
A���A�p�A� �A��A��9A���A��mA���A�(�A��A�/A�r�A���A��A�oA�|�A�M�A�;A}G�A{dZAyƨAuAq&�Ap�HAq"�Ap�!Ao33Am+AljAk�;Ai"�Af9XAb�9A` �A]ƨA\�9A[%AYƨAXbNAUdZAS&�ARA�AQ
=AO�AM/AJbNAH��AG�AF�+AE��AEO�AD�\ACoABI�AA�7A?�hA=��A<��A:9XA8I�A6A�A5VA4E�A21A0�jA0bNA.��A-p�A-&�A,�A,�jA+\)A)��A(M�A%��A$ZA#�A!�
A ZA7LA{AK�A1'A��A�AI�A��A�yA��AdZAffA7LA1'AĜA�A�yA��A�HAM�A;dA�!AbAdZA
��A
A�A��A/A��A�A�A�wA 5?@��@���@�`B@���@�r�@��@��H@��@�1'@�r�@��P@�G�@�@��@�n�@�5?@�G�@��@�;d@�D@땁@�&�@�  @�o@�-@�z�@�O�@�9X@߅@ޟ�@��@��@��H@ڏ\@��#@ش9@ו�@���@�$�@�x�@���@�z�@�Z@���@ӕ�@��y@��@��;@�;d@θR@͙�@̬@�1'@ˍP@ʧ�@�r�@�33@�=q@���@î@��H@�@�b@��@��@�G�@���@�j@���@�\)@���@�{@��@�x�@�Ĝ@��D@��@��y@�l�@��@�@�`B@��`@���@��m@�S�@�ȴ@�=q@��7@��D@�(�@�ƨ@�C�@�
=@��y@���@�ff@���@���@�%@��D@�I�@�9X@�(�@��F@�ȴ@�=q@��@��@��-@�hs@�?}@��@�bN@�b@���@���@�"�@���@�J@�@�^5@�ff@�-@��-@��h@�hs@�?}@��D@�1@�ƨ@��@��P@���@��@���@��R@��\@�{@�@���@���@��^@�V@�I�@�b@��m@���@���@��@��H@�
=@�n�@�^5@��T@�p�@�?}@�&�@���@��@��/@�Ĝ@��@�Z@� �@�ƨ@�|�@�;d@���@�ȴ@�~�@�M�@�-@��@�@�@���@���@��h@��@�hs@�O�@�/@�V@��j@�r�@�Q�@�1'@��m@��@��@�l�@�\)@�S�@�K�@�K�@�;d@�o@��H@���@�-@���@�`B@�7L@���@��j@�j@�1'@���@���@���@�t�@�33@��@�o@�o@���@��y@��@��R@�V@�5?@��@���@�G�@���@���@�1'@�  @��;@��@�dZ@�o@��y@��R@���@���@���@��!@��R@��!@�n�@�-@�{@���@��#@���@�`B@�O�@�&�@���@�A�@��
@���@���@���@�dZ@�+@���@�{@��#@��h@�hs@�O�@�7L@�@�l�@}@o�;@hQ�@a��@Yx�@P��@J^5@B^5@<1@4��@.$�@'K�@#C�@�@��@��@�@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBS�BR�BR�BR�BS�BS�BR�BR�BR�BR�BR�BS�BS�BS�BS�BR�BW
BZB]/B`BBdZBe`B�B� Bv�Bq�Bq�Bt�B~�B{�BiyBl�Bt�B�DB�bB�{B��B�?BȴB��B��B�wB�/B��B+B{B�B!�B7LB>wBI�BP�BXBbNBffBjBw�B�B�VB�DB�1B�B�bB�\B�PB�JB�PB�VB�hB�PB��B��B�B�B�3B��B��B��B��B�VB�=B�B{�Bt�Bo�Be`B\)BP�B49B0!B?}BA�B?}B<jB8RB49B5?B/B)�B1'B)�B�BBB��B�HB�!Bz�B]/BS�BM�BF�B49B�B1B
�fB
��B
��B
�B
��B
�JB
~�B
gmB
D�B
(�B
�B
uB
B	��B	��B	�B	�B	�B	��B	��B	�^B	�HB	�fB	�B	��B	�B	��B	��B	�B	��B	�DB	~�B	{�B	y�B	s�B	iyB	ZB	L�B	G�B	?}B	8RB	/B	(�B	!�B	�B	�B	oB	\B		7B	B��B��B�B�B�sB�HB�
B��BɺBŢB��B�qB�^B�LB�?B�9B�3B�-B�B��B��B��B��B��B��B�{B�oB�hB�\B�PB�DB�=B�7B�1B�1B�B�B� B|�B{�By�Bv�Bt�Br�Bq�Bo�Bo�Bn�Bm�Bl�Bk�BiyBgmBffBdZBbNB_;B^5BbNBffBp�Br�Br�Bt�Bs�Bp�Bx�B�VB�{B��B��B�{B��B�{B�uB�uB�oB�hB�bB�VB�bB�hB�oB�oB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�'B�9B�RB�jB�wB��BŢBǮBɺB��B��B��B�B�B�B�)B�5B�;B�;B�ZB�yB�B�B�B�B�B��B��B��B��B	B	DB	\B	uB	�B	�B	�B	�B	�B	!�B	$�B	,B	2-B	6FB	8RB	9XB	;dB	<jB	=qB	A�B	H�B	J�B	M�B	M�B	M�B	Q�B	S�B	W
B	W
B	\)B	`BB	dZB	gmB	jB	l�B	l�B	m�B	r�B	s�B	t�B	v�B	w�B	w�B	x�B	}�B	�B	�B	�B	�B	�B	�%B	�=B	�VB	�\B	�bB	�oB	�bB	�bB	�bB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�-B	�3B	�3B	�9B	�FB	�RB	�XB	�^B	�^B	�dB	�dB	�jB	�qB	�wB	�wB	�}B	�}B	��B	ÖB	ĜB	ŢB	ǮB	ȴB	ȴB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�#B	�)B	�)B	�5B	�BB	�HB	�NB	�NB	�TB	�ZB	�`B	�mB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
+B

=B
uB
�B
&�B
,B
1'B
7LB
>wB
C�B
H�B
M�B
S�B
ZB
`BB
dZB
hsB
k�B
o�B
s�B
u�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BS�BR�BR�BR�BS�BS�BR�BR�BR�BR�BR�BS�BS�BS�BS�BR�BWBZ#B]8B`GBd`BegB�B�Bv�Bq�Bq�Bt�B B{�BiBl�Bt�B�KB�hB��B��B�CBȷB��B��B�{B�8B��B.B�B�B!�B7PB>}BI�BP�BXBbQBflBj�Bw�B�B�ZB�IB�7B� B�hB�aB�WB�PB�WB�_B�lB�VB��B��B�B�B�9B��B��B��B��B�[B�CB�B{�Bt�Bo�BeeB\.BP�B4=B0'B?�BA�B?�B<qB8VB4AB5DB/ B*B1-B* B�BBB��B�MB�(Bz�B]6BS�BM�BF�B4>B�B7B
�mB
��B
��B
�B
��B
�UB
B
gxB
D�B
)B
�B
�B
'B	��B	��B	�B	�B	�B	��B	��B	�oB	�UB	�uB	�$B	�B	�B	��B	��B	�&B	��B	�VB	B	{�B	y�B	s�B	i�B	Z2B	L�B	G�B	?�B	8gB	//B	)B	!�B	�B	�B	�B	tB		MB	$B�B��B��B�B�B�bB�#B��B��BŹB��B��B�yB�fB�\B�TB�NB�HB�8B�B��B��B��B��B��B��B��B��B�yB�nB�cB�XB�UB�MB�OB�:B�+B�B}B|By�Bv�Bt�Br�Bq�Bo�Bo�Bn�Bm�Bl�Bk�Bi�Bg�Bf�BdyBboB_ZB^QBblBf�Bp�Br�Br�Bt�Bs�Bp�Bx�B�rB��B��B��B��B��B��B��B��B��B��B�B�qB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�	B��B��B�B�B�B�B�'B�4B�@B�UB�iB��B��B��BŻB��B��B��B�B�B�(B�5B�4B�AB�NB�UB�SB�rB�B�B�B��B��B��B��B��B��B�B	'B	[B	pB	�B	�B	�B	�B	�B	�B	!�B	$�B	,B	2?B	6YB	8gB	9lB	;yB	<~B	=�B	A�B	H�B	J�B	M�B	M�B	M�B	R B	TB	WB	WB	\=B	`UB	dmB	g�B	j�B	l�B	l�B	m�B	r�B	s�B	t�B	v�B	w�B	w�B	x�B	~B	�B	�%B	�'B	�%B	�$B	�5B	�PB	�iB	�mB	�tB	�B	�tB	�uB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�$B	�-B	�8B	�?B	�FB	�FB	�KB	�VB	�bB	�fB	�oB	�nB	�wB	�uB	�yB	��B	��B	��B	��B	��B	��B	æB	ĭB	űB	ǿB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�&B	�.B	�.B	�.B	�4B	�:B	�8B	�FB	�RB	�XB	�]B	�^B	�dB	�hB	�pB	�{B	�}B	�B	�B	�B	�B	��B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B
 B
 B
B
B
 B
,B
-B
,B
,B
-B
,B
-B
+B
;B

JB
�B
�B
&�B
,B
14B
7ZB
>�B
C�B
H�B
M�B
TB
Z)B
`OB
dfB
h~B
k�B
o�B
s�B
u�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.06 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214252016053112142520160531121425  AO  ARCAADJP                                                                    20140721230509    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230509  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230509  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121425  IP                  G�O�G�O�G�O�                