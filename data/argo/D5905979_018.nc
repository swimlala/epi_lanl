CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:08:56Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200619170856  20220204114412  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @؈
=�1   @؈����@7���vȴ�c��t�j1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @���@�  A   A   A@  A`  A�  A���A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B_��Bh  Bp  Bx  B�  B�  B�  B�  B�  B�33B���B�ffB�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C�fC  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2�C433C5��C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl�Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5�fD6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DG��DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DUy�DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDy�{D��HD�[�D��{D��)D�qD�[�D�� D��=D�
D�MD���D�ӅD�(�D�VDژ�D��D�!HD�_\D�qD��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��H@�{@�{A
=A?
=A_
=A
=A�Q�A��A��A��AυA߅A�A��BBBBB'B/B7B?BGBOBWB_\)BgBoBwBB��HB��HB��HB��HB�{B��B�G�B��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HC�C�C�C�C	�C�C�C�C�C�
C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C2
>C4#�C5�>C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Cl
>Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��C��RC��RC��RC��RC��RC��RC��RC��RC��RC�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RD |)D �)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D	|)D	�)D
|)D
�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)Du�D�)D|)D�)D|)D�)D|)D�)D��D�)D|)D�)D|)D�)D|)D�)D|)D�)D |)D �)D!|)D!�)D"|)D"�)D#|)D#�)D$|)D$�)D%|)D%�)D&|)D&�)D'|)D'�)D(|)D(�)D)|)D)�)D*|)D*�)D+|)D+�)D,|)D,�)D-|)D-�)D.|)D.�)D/|)D/�)D0|)D0�)D1|)D1�)D2|)D2�)D3|)D3�)D4|)D4�)D5��D5�)D6|)D6�)D7|)D7�)D8|)D8�)D9|)D9�)D:|)D:�)D;|)D;�)D<|)D<�)D=|)D=�)D>|)D>�)D?|)D?�)D@|)D@�)DA|)DA�)DB|)DB�)DC|)DC�)DD|)DD�)DE|)DE�)DF|)DF�)DG|)DG��DH|)DH�)DI|)DI�)DJ|)DJ�)DK|)DK�)DL|)DL�)DM|)DM�)DN|)DN�)DO|)DO�)DP|)DP�)DQ|)DQ�)DR|)DR�)DS|)DS�)DT|)DT�)DUu�DU�)DV|)DV�)DW|)DW�)DX|)DX�)DY|)DY�)DZ|)DZ�)D[|)D[�)D\|)D\�)D]|)D]�)D^|)D^�)D_|)D_�)D`|)D`�)Da|)Da�)Db|)Db�)Dc|)Dc�)Dd|)Dd�)De|)De�)Df|)Df�)Dg|)Dg�)Dh|)Dh�)Di|)Di�)Dj|)Dj�)Dk|)Dk�)Dl|)Dl�)Dm|)Dm�)Dn|)Dn�)Do|)Do�)Dp|)Dp�)Dq|)Dq�)Dr|)Dr�)Ds|)Ds�)Dt|)DtDy��D��\D�Y�D���D��=D��D�Y�D��D��QD�D�K3D���D�љD�'
D�T(Dڗ
D��D�\D�]pD�D��(111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Aԩ�AԮA԰!AԶFAԴ9AԴ9AԼjAԣ�AԁA�|�A�v�A�r�A�l�A�^5A�^5A�Q�A��A��TA��HA��HA��/A���A���A���A���AӺ^Aӗ�A�$�AҴ9A���A�l�Aϴ9A��A̲-Aʥ�A�S�A�XA���A��
A��yA��-A��!A�x�A��wA�^5A� �A�`BA�bNA�K�A��A���A���A���A�ZA��hA���A�A��DA�VA��FA�~�A�33A�C�A���A�7LA�5?A��FA�=qA�1'A��!A�v�A��A���A�A�=qA�1A���A��+A�C�A���A��A��`A� �A��9A���A��9A���A��hA�K�A��A��DA�bNA��HA�VA�ĜA�
=A�K�A��;A��A|z�Az9XAwdZAt�At1As%Aq�#AnffAlZAk/Ai�TAg�-Ad�RAc�
Ac�wAbjA`5?A^-A\��A\�A["�AY�#AX��AW��AV�9AUK�AT9XAR�9AOƨAMAJ�AIAG�AFVAE33AC��AC7LAAG�A?
=A<�9A;�A;A;|�A:��A:1A8�!A8  A7�-A7l�A6��A5�A4�A3S�A2�A1VA09XA/��A.�A,�!A*�A(9XA'&�A&M�A%�A%7LA$v�A$bA#ƨA#/A!��A ��A �A A��AS�A��AA��A\)A�A��A$�A��AjA=qA�A\)A��A�\Al�A\)AG�AĜA�AG�A�HA�DAJA+AS�A9XAK�A�Az�A�PA��A��A�;A
=A	��Av�A�A�A"�A�A�AG�A�9A(�A n�@�|�@���@���@��@�@��9@�|�@��+@��-@�?}@���@�ȴ@�p�@�Z@��
@�|�@�l�@�;d@�-@�x�@�Ĝ@�u@�r�@��@�-@畁@�o@�5?@�Ĝ@�F@��H@�G�@�I�@��@��#@�A�@�`B@�7L@�Ĝ@׶F@�+@���@�J@��@�l�@ёh@��@�33@�V@ͺ^@�&�@�Z@���@�S�@ɩ�@�(�@�5?@�V@�Z@�(�@�  @�V@��7@�&�@�z�@��;@�
=@��T@��@��@�r�@�(�@���@���@���@��h@�O�@��9@��@���@�@�~�@��@�/@�9X@��w@���@��@�b@��y@��@�p�@��@�b@��@�E�@��/@��P@���@��#@�Ĝ@�1@��w@�dZ@�;d@���@��T@�X@���@��D@��@���@�K�@��y@�v�@��+@�v�@�n�@�^5@�E�@�$�@���@�X@�&�@���@��`@���@� �@�b@�9X@�1'@��@��@�K�@��\@��\@�^5@��+@�~�@���@�`B@�p�@���@���@��#@�@�G�@�V@�%@���@���@��@��@�1@�1@���@�S�@�@���@���@���@���@�n�@�E�@��@�p�@�O�@�G�@���@�j@�1'@��;@�ƨ@���@��;@��@��;@��F@�|�@�dZ@�+@���@�=q@��-@��h@��h@��7@�p�@�`B@�7L@�?}@�G�@�?}@�?}@�7L@���@��D@�z�@�r�@�r�@�Q�@��@��;@�C�@���@���@�=q@��@���@���@�p�@�%@��`@���@��u@�j@�j@�Z@�bN@�r�@�bN@�1'@�(�@� �@� �@�b@��m@��@�|�@�dZ@�S�@�@�ȴ@���@���@���@���@���@�~�@�-@��@�p�@�&�@���@��u@��D@�z�@�Z@�Z@�A�@�9X@�(�@�  @��m@��w@��P@�t�@�C�@���@�5?@�J@��@���@�&�@���@��`@��9@���@�r�@��m@��@{�[@rJ@f	@`��@Z��@UO�@M8�@E�@@��@9��@5�T@/]�@)ԕ@%�@ϫ@q@�v@�r@�/@�P111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  Aԩ�AԮA԰!AԶFAԴ9AԴ9AԼjAԣ�AԁA�|�A�v�A�r�A�l�A�^5A�^5A�Q�A��A��TA��HA��HA��/A���A���A���A���AӺ^Aӗ�A�$�AҴ9A���A�l�Aϴ9A��A̲-Aʥ�A�S�A�XA���A��
A��yA��-A��!A�x�A��wA�^5A� �A�`BA�bNA�K�A��A���A���A���A�ZA��hA���A�A��DA�VA��FA�~�A�33A�C�A���A�7LA�5?A��FA�=qA�1'A��!A�v�A��A���A�A�=qA�1A���A��+A�C�A���A��A��`A� �A��9A���A��9A���A��hA�K�A��A��DA�bNA��HA�VA�ĜA�
=A�K�A��;A��A|z�Az9XAwdZAt�At1As%Aq�#AnffAlZAk/Ai�TAg�-Ad�RAc�
Ac�wAbjA`5?A^-A\��A\�A["�AY�#AX��AW��AV�9AUK�AT9XAR�9AOƨAMAJ�AIAG�AFVAE33AC��AC7LAAG�A?
=A<�9A;�A;A;|�A:��A:1A8�!A8  A7�-A7l�A6��A5�A4�A3S�A2�A1VA09XA/��A.�A,�!A*�A(9XA'&�A&M�A%�A%7LA$v�A$bA#ƨA#/A!��A ��A �A A��AS�A��AA��A\)A�A��A$�A��AjA=qA�A\)A��A�\Al�A\)AG�AĜA�AG�A�HA�DAJA+AS�A9XAK�A�Az�A�PA��A��A�;A
=A	��Av�A�A�A"�A�A�AG�A�9A(�A n�@�|�@���@���@��@�@��9@�|�@��+@��-@�?}@���@�ȴ@�p�@�Z@��
@�|�@�l�@�;d@�-@�x�@�Ĝ@�u@�r�@��@�-@畁@�o@�5?@�Ĝ@�F@��H@�G�@�I�@��@��#@�A�@�`B@�7L@�Ĝ@׶F@�+@���@�J@��@�l�@ёh@��@�33@�V@ͺ^@�&�@�Z@���@�S�@ɩ�@�(�@�5?@�V@�Z@�(�@�  @�V@��7@�&�@�z�@��;@�
=@��T@��@��@�r�@�(�@���@���@���@��h@�O�@��9@��@���@�@�~�@��@�/@�9X@��w@���@��@�b@��y@��@�p�@��@�b@��@�E�@��/@��P@���@��#@�Ĝ@�1@��w@�dZ@�;d@���@��T@�X@���@��D@��@���@�K�@��y@�v�@��+@�v�@�n�@�^5@�E�@�$�@���@�X@�&�@���@��`@���@� �@�b@�9X@�1'@��@��@�K�@��\@��\@�^5@��+@�~�@���@�`B@�p�@���@���@��#@�@�G�@�V@�%@���@���@��@��@�1@�1@���@�S�@�@���@���@���@���@�n�@�E�@��@�p�@�O�@�G�@���@�j@�1'@��;@�ƨ@���@��;@��@��;@��F@�|�@�dZ@�+@���@�=q@��-@��h@��h@��7@�p�@�`B@�7L@�?}@�G�@�?}@�?}@�7L@���@��D@�z�@�r�@�r�@�Q�@��@��;@�C�@���@���@�=q@��@���@���@�p�@�%@��`@���@��u@�j@�j@�Z@�bN@�r�@�bN@�1'@�(�@� �@� �@�b@��m@��@�|�@�dZ@�S�@�@�ȴ@���@���@���@���@���@�~�@�-@��@�p�@�&�@���@��u@��D@�z�@�Z@�Z@�A�@�9X@�(�@�  @��m@��w@��P@�t�@�C�@���@�5?@�J@��@���@�&�@���@��`@��9@���@�r�G�O�@��@{�[@rJ@f	@`��@Z��@UO�@M8�@E�@@��@9��@5�T@/]�@)ԕ@%�@ϫ@q@�v@�r@�/@�P111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBŢBŢBŢBĜBĜBŢBƨB��B��B��B��B��B�
B�5B�;B�sBJB{B�B�B�B�B�B�B�B�B�B �B$�B-B<jBB�BI�BdZBu�Bl�BffBjB�+B�VB��B��B�uB�{B��B�bB�%B�B|�Bx�Bs�Bm�Be`BbNB\)BT�BJ�BG�B?}B;dB7LB33B,BPB�B�5B��BȴBĜB�qB�B�hBz�BjBR�B:^B2-B.B'�B!�BoBB
��B
�`B
�
B
��B
��B
��B
ǮB
ĜB
�B
��B
�hB
�B
{�B
n�B
aHB
O�B
&�B
+B	��B	�NB	��B	ŢB	�wB	�FB	��B	�VB	�B	y�B	n�B	[#B	N�B	L�B	H�B	:^B	.B	%�B	�B	�B	oB	PB	B��B��B�B�fB��BĜB�^B��B��B��B��B�uB�oB�hB�=B�1B�hB��B��B��B��B��B��B��B��B�B�B�B��B��B��B��B��B��B��B�\B�B|�By�Bv�Bt�Br�Bq�Bp�Bn�BjBgmBhsBgmBffBe`Be`BcTBaHBcTBbNBaHB`BB]/BXBW
BT�BW
BQ�BO�BR�BT�BT�BT�BT�BS�BT�BW
BXBVBN�BI�BC�BB�BA�BC�BB�BA�B@�B=qB=qB?}BB�BA�BA�BA�BA�B@�B=qB<jB<jB:^B9XB:^B8RB8RB8RB8RB7LB7LB6FB7LB6FB7LB7LB7LB7LB7LB7LB7LB8RB8RB8RB7LB7LB9XB9XB8RB:^B<jB<jB:^B8RB9XB8RB9XB<jB=qB=qB=qB=qB>wB>wB?}BA�BH�BL�BI�BK�BP�BP�BP�BR�BR�BS�BVBXB\)B]/B_;B_;B^5BbNBbNBbNBcTBcTBdZBe`BffBgmBgmBgmBhsBjBl�Bl�Bm�Bo�Bo�Bp�Br�Bs�Bt�Bu�Bw�Bx�B{�B~�B�B�B�%B�1B�1B�JB�\B�oB��B��B��B��B��B�B�B�B�B�-B�LB�XB�dB�qB�}B��BBƨB��B��B��B��B��B�B�B�B�#B�)B�/B�HB�TB�`B�ZB�fB�sB�B�B��B��B��B��B��B��B	B	B	1B	VB	uB	uB	{B	�B	�B	�B	�B	�B	�B	$�B	(�B	+B	/B	33B	5?B	8RB	;dB	;dB	;dB	?}B	A�B	D�B	J�B	K�B	L�B	S�B	YB	ZB	^5B	_;B	`BB	bNB	cTB	dZB	e`B	gmB	hsB	iyB	k�B	m�B	o�B	p�B	r�B	v�B	w�B	y�B	y�B	{�B	~�B	� B	� B	�B	�B	�+B	�7B	�7B	�7B	�DB	�JB	�VB	�hB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�-B	�?B	�?B	�?B	�?B	�FB	�RB	�jB	�qB	�qB	�qB	��B	B	B	ÖB	ĜB	ĜB	ÖB	ĜB	ǮB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�5B	�5B	�5B	�5B	�;B	�HB	�ZB	�fB	�IB	��B
�B
&B
�B
 'B
)_B
2�B
9rB
?�B
F�B
J#B
O�B
T�B
ZB
a�B
gB
kkB
oOB
p�B
t�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B��B��B��B��B��B��B��B��B��B��B��B�B�B�@B�FB�}BRB�B�B�B�B�B�B�B�B�B�B�B�B$B3oB9�B@�B[]Bl�Bc�B]jBa�B~.B�YB��B��B�yB�B��B�gB}+BzBs�Bo�Bj�Bd�B\iBYWBS3BL	BA�B>�B6�B2qB.YB*@B#B`B�B�IB�B��B��B��B�&B��Bq�Ba�BJB1B)NB%6BB�B	�B
�+B
��B
܇B
�2B
�B
��B
��B
��B
��B
�@B
��B
��B
zAB
sB
e�B
XzB
GB
B	�dB	�	B	يB	�B	��B	��B	��B	�B	��B	|bB	qB	e�B	RjB	F!B	DB	?�B	1�B	%^B	.B		B	�B		�B	�B�gB�HB�B��BݶB�7B��B��B�QB�-B�B��B��B��B��B��B�B��B��B��B��B�.B�.B�:B�GB�SB�YB�kB�YB�/B�/B�)B�)B�*B�B��B��B{tBtJBq7Bn&BlBjBiBhBe�Ba�B^�B_�B^�B]�B\�B\�BZ�BX�BZ�BY�BX�BW�BT�BOpBNjBL^BNjBIMBG@BJSBL_BL_BL_BL_BKYBL_BNkBOqBMeBF;BAB:�B9�B8�B:�B9�B8�B7�B4�B4�B6�B9�B8�B8�B8�B8�B7�B4�B3�B3�B1�B0�B1�B/�B/�B/�B/�B.�B.�B-�B.�B-�B.�B.�B.�B.�B.�B.�B.�B/�B/�B/�B.�B.�B0�B0�B/�B1�B3�B3�B1�B/�B0�B/�B0�B3�B4�B4�B4�B4�B5�B5�B6�B8�B@BD4BA!BC.BHLBHLBHLBJYBJYBK_BMkBOwBS�BT�BV�BV�BU�BY�BY�BY�BZ�BZ�B[�B\�B]�B^�B^�B^�B_�Ba�Bc�Bc�Bd�BgBgBh
BjBkBl"Bm)Bo5Bp;BsMBv`ByrB{B}�B�B�B��B��B��B��B�B�B�5B�MB�fB�lB�B�B��B��B��B��B��B��B��B��B�
B�#B�/B�AB�GB�TB�eB�qB�xB҄BӊBԐBةBڵB��BۻB��B��B��B�B�B�(B�(B�.B�@B�SB�xB�~B��B	�B	
�B	
�B	�B	�B	�B	B	B	B	B	:B	 RB	"^B	&wB	*�B	,�B	/�B	2�B	2�B	2�B	6�B	8�B	;�B	BB	C!B	D'B	KRB	PpB	QvB	U�B	V�B	W�B	Y�B	Z�B	[�B	\�B	^�B	_�B	`�B	b�B	d�B	f�B	g�B	jB	n!B	o'B	q2B	q2B	s>B	vQB	wWB	wWB	x]B	ziB	~�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�,B	�>B	�EB	�KB	�\B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	� B	�&B	�+B	�1B	�1B	�7B	�7B	�7B	�>B	�DB	�JB	�PB	�PB	�VB	�bB	�iB	�iB	�iB	ՇB	ՇB	ՇB	ՇB	֍B	ؚB	۫G�O�B	�B	�7B	��B

uB
B
vB
 �B
)�B
0�B
74B
=�B
AqB
F�B
K�B
QjB
Y2B
^kB
b�B
f�B
h
B
l111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.06 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.009(+/-0.002) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144122022020411441220220204114412  AO  ARCAADJP                                                                    20200619170856    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170856  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170856  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114412  IP                  G�O�G�O�G�O�                