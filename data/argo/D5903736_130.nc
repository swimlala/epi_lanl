CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-11-14T03:15:48Z AOML 3.0 creation; 2016-05-31T19:14:46Z UW 3.1 conversion     
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
resolution        :�o     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gx   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ix   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qp   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �`   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �X   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �(   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �,   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �0   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20151114031548  20160531121446  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051_7090_130                   2C  D   APEX                            5368                            041511                          846 @�~o���]1   @�~p7��}@3���+�df��"��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   A   A   @,��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A���A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  BxffB~��B���B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DWy�DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dy� D�fD�33D�i�D�ɚD�3D�C3D�9�D�ɚD�� D�0 D�l�D�� D�fD�33Dڐ D��fD��3D�6fD�\�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @(��@|(�@�{@�{A
=A?
=A_
=A
=A��A��A��A�Q�AυA߅A�A��BBBBB'B/B7B?BGBOBWB_BgBoBx(�B~�\B��B��HB��HB��HB��HB��HB�{B��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HC�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RD |)D �)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D	|)D	�)D
|)D
�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D |)D �)D!|)D!�)D"|)D"�)D#|)D#�)D$|)D$�)D%|)D%�)D&|)D&�)D'|)D'�)D(|)D(�)D)|)D)�)D*|)D*�)D+|)D+�)D,|)D,�)D-|)D-�)D.|)D.�)D/|)D/�)D0|)D0�)D1|)D1�)D2|)D2�)D3|)D3�)D4|)D4�)D5|)D5�)D6|)D6�)D7|)D7�)D8|)D8�)D9|)D9�)D:|)D:�)D;|)D;�)D<|)D<�)D=|)D=�)D>|)D>�)D?|)D?�)D@|)D@�)DA|)DA�)DB|)DB�)DC|)DC�)DD|)DD�)DE|)DE�)DF|)DF�)DG|)DG�)DH|)DH�)DI|)DI�)DJ|)DJ�)DK|)DK�)DL|)DL�)DM|)DM�)DN|)DN�)DO|)DO�)DP|)DP�)DQ|)DQ�)DR|)DR�)DS|)DS�)DT|)DT�)DU|)DU�)DV|)DV�)DWu�DW�)DX|)DX�)DY|)DY�)DZ|)DZ�)D[|)D[�)D\|)D\�)D]|)D]�)D^|)D^�)D_|)D_�)D`|)D`�)Da|)Da�)Db|)Db�)Dc|)Dc�)Dd|)Dd�)De|)De�)Df|)Df�)Dg|)Dg�)Dh|)Dh�)Di|)Di�)Dj|)Dj�)Dk|)Dk�)Dl|)Dl�)Dm|)Dm�)Dn|)Dn�)Do|)Do�)Dp|)Dp�)Dq|)Dq�)Dr|)Dr�)Ds|)Ds�)Dtu�Dy�)D�zD�1GD�g�D�ǮD�GD�AGD�7�D�ǮD��D�.D�j�D��D�zD�1GDڎD��zD��GD�4zD�Z�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A��TA��TA��`A��`A��TA���A���AۼjA�~�A�x�A�n�A�|�A�oAڴ9A�jA�;dA�33A�"�A�A���A٩�A�v�A�33A��A�t�A׶FA�l�A�C�A�+A�;dAӟ�AϓuA�A�I�A�+A�{A�bNA��A���A�`BA��\A�O�A��^A�n�A���A� �A�A�;dA�O�A���A��!A���A�ƨA���A�$�A��#A�jA�VA��wA�&�A���A���A�l�A�5?A�{A�7LA�K�A��7A�VA��A��^A��A��A�-A��A�A���A�E�A�E�A�A�
=A�hsA� �A��wA��mA���A�~�A���A�  A���A�n�A�%A���A�A�A��A���A�
=A���A�E�A�G�A7LA}ƨA}XAz{Aw?}Av�At1Ap(�An~�Al�Aj��Ahr�Ae�-Ab�/A`�A^�9A\�uAZ(�AX�yAX$�AV�AU
=AR�+AQ
=AO��AN��AM��AM;dAL��AK;dAH�+AF��AE��AC�7AAƨA@�A?�PA>�/A>9XA<��A;oA:5?A9�mA89XA5�-A2�A/��A.{A,�A++A)�FA(ZA'�A'O�A&jA$z�A"��A!G�A ��A   A��Az�A��AC�A{A��Ap�A��A��A��A�A�FA�A �A�A��A5?A�A
�!A
 �A	S�A��A�AC�A�/A�Ax�A�A|�AĜA��A9XAC�A -@�O�@��@���@�Ĝ@��D@�t�@��\@��@�O�@��@�v�@�@��;@���@�V@�x�@���@���@�"�@��#@�Ĝ@�1'@�P@��@��@��@��@�P@��@��D@�C�@ݙ�@�  @�S�@ڸR@�@�%@�1@׍P@���@�E�@�$�@�J@��#@�j@�S�@ҧ�@�J@�X@�z�@�\)@ͩ�@�I�@��
@�S�@�ȴ@�{@�bN@�\)@�ȴ@���@���@��@Ɵ�@��T@��/@�1@+@��@�x�@���@�Ĝ@�A�@�K�@���@���@�Z@���@�A�@��u@��m@�S�@�ȴ@���@�M�@�@�O�@��@�Ĝ@��@�9X@���@��@�S�@�+@��@�@�?}@���@�A�@�b@��F@��@��\@�@��@��j@�r�@�Z@���@�|�@�
=@�n�@��#@��h@�/@��j@�j@� �@��@��m@��m@�|�@�ȴ@��@�G�@���@�hs@�hs@��`@�A�@�Z@�I�@� �@��w@��P@�K�@��y@�ȴ@�v�@��@���@��7@��@��@���@��@�A�@�9X@��F@�\)@�
=@���@���@��!@�v�@�M�@��@��^@���@�hs@�/@�V@��@��j@��D@��@���@�S�@�+@�
=@��@��R@��+@��@�J@���@��u@�o@�|�@�C�@���@��T@���@���@�%@��9@��m@���@��@�  @�b@� �@��
@�33@��@��@�
=@�"�@�"�@�\)@�C�@��!@�^5@�M�@�ff@��^@��@��@��j@��@�S�@���@�^5@�J@���@��@��#@�E�@�ff@���@��7@��h@�O�@���@���@�+@���@���@�Z@�z�@�r�@�bN@�(�@��m@���@�;d@�+@��@�ff@�=q@���@�`B@��@���@��@�/@�X@��@���@���@� �@�ƨ@��@���@���@��;@��@��@�ff@���@���@�X@���@��@�z�@��;@�ƨ@��P@�;d@��@��H@��+@�@���@��h@�`B@�?}@�G�@��@��@�j@�1@��@�\)@�"�@�@�ȴ@�~�@�~�@�$�@��#@�@���@~�@v��@o|�@fȴ@]��@S��@K@D�@=�@7�@0A�@*n�@%�h@ ��@�@�@(�@1'@�-@
�H@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A��TA��TA��`A��`A��TA���A���AۼjA�~�A�x�A�n�A�|�A�oAڴ9A�jA�;dA�33A�"�A�A���A٩�A�v�A�33A��A�t�A׶FA�l�A�C�A�+A�;dAӟ�AϓuA�A�I�A�+A�{A�bNA��A���A�`BA��\A�O�A��^A�n�A���A� �A�A�;dA�O�A���A��!A���A�ƨA���A�$�A��#A�jA�VA��wA�&�A���A���A�l�A�5?A�{A�7LA�K�A��7A�VA��A��^A��A��A�-A��A�A���A�E�A�E�A�A�
=A�hsA� �A��wA��mA���A�~�A���A�  A���A�n�A�%A���A�A�A��A���A�
=A���A�E�A�G�A7LA}ƨA}XAz{Aw?}Av�At1Ap(�An~�Al�Aj��Ahr�Ae�-Ab�/A`�A^�9A\�uAZ(�AX�yAX$�AV�AU
=AR�+AQ
=AO��AN��AM��AM;dAL��AK;dAH�+AF��AE��AC�7AAƨA@�A?�PA>�/A>9XA<��A;oA:5?A9�mA89XA5�-A2�A/��A.{A,�A++A)�FA(ZA'�A'O�A&jA$z�A"��A!G�A ��A   A��Az�A��AC�A{A��Ap�A��A��A��A�A�FA�A �A�A��A5?A�A
�!A
 �A	S�A��A�AC�A�/A�Ax�A�A|�AĜA��A9XAC�A -@�O�@��@���@�Ĝ@��D@�t�@��\@��@�O�@��@�v�@�@��;@���@�V@�x�@���@���@�"�@��#@�Ĝ@�1'@�P@��@��@��@��@�P@��@��D@�C�@ݙ�@�  @�S�@ڸR@�@�%@�1@׍P@���@�E�@�$�@�J@��#@�j@�S�@ҧ�@�J@�X@�z�@�\)@ͩ�@�I�@��
@�S�@�ȴ@�{@�bN@�\)@�ȴ@���@���@��@Ɵ�@��T@��/@�1@+@��@�x�@���@�Ĝ@�A�@�K�@���@���@�Z@���@�A�@��u@��m@�S�@�ȴ@���@�M�@�@�O�@��@�Ĝ@��@�9X@���@��@�S�@�+@��@�@�?}@���@�A�@�b@��F@��@��\@�@��@��j@�r�@�Z@���@�|�@�
=@�n�@��#@��h@�/@��j@�j@� �@��@��m@��m@�|�@�ȴ@��@�G�@���@�hs@�hs@��`@�A�@�Z@�I�@� �@��w@��P@�K�@��y@�ȴ@�v�@��@���@��7@��@��@���@��@�A�@�9X@��F@�\)@�
=@���@���@��!@�v�@�M�@��@��^@���@�hs@�/@�V@��@��j@��D@��@���@�S�@�+@�
=@��@��R@��+@��@�J@���@��u@�o@�|�@�C�@���@��T@���@���@�%@��9@��m@���@��@�  @�b@� �@��
@�33@��@��@�
=@�"�@�"�@�\)@�C�@��!@�^5@�M�@�ff@��^@��@��@��j@��@�S�@���@�^5@�J@���@��@��#@�E�@�ff@���@��7@��h@�O�@���@���@�+@���@���@�Z@�z�@�r�@�bN@�(�@��m@���@�;d@�+@��@�ff@�=q@���@�`B@��@���@��@�/@�X@��@���@���@� �@�ƨ@��@���@���@��;@��@��@�ff@���@���@�X@���@��@�z�@��;@�ƨ@��P@�;d@��@��H@��+@�@���@��h@�`B@�?}@�G�@��@��@�j@�1@��@�\)@�"�@�@�ȴ@�~�@�~�@�$�@��#@�@���@~�@v��@o|�@fȴ@]��@S��@K@D�@=�@7�@0A�@*n�@%�h@ ��@�@�@(�@1'@�-@
�H@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
�`B
�ZB
�ZB
�TB
�TB
�`B
�B
��B
��B�B�B�B�BC�Bl�B��B��B�B�TB��B�B(�B?}BjB~�B��BɺB��B�B�)B�BoB,B<jB=qBA�BH�BF�BJ�BS�BR�BS�BT�BXB[#BQ�BL�BM�BM�BH�B?}B0!B#�B�B�B�BVB%B��B��B�B��B��B�}B�^B�FB��B��Bu�B^5BW
BP�B5?B!�B�B��B�?B��B��Bu�B]/BP�B`BBw�Bz�Bq�BaHBJ�B;dB/B&�B�BB
��B
��B
�B
�B
�)B
��B
�}B
�B
��B
�uB
�7B
gmB
F�B
F�B
7LB
hB
B	��B	�/B	ǮB	�'B	��B	�bB	�B	x�B	q�B	k�B	dZB	YB	L�B	>wB	6FB	/B	+B	&�B	"�B	�B	�B	bB	
=B	B��B�B�B�B�B�mB�HB�)B�B�B��BĜB�dB�?B�B��B��B��B��B��B��B��B�{B�\B�JB�DB�1B�+B�B�B|�By�Bv�Bu�Bt�Bs�Bq�Bn�Bo�Bp�Bs�Bs�Bt�Bt�Bq�Bp�Bn�Bn�Bq�Bo�Bn�Bl�Bk�BjBhsBgmBiyBjBjBhsBgmBl�Bn�Bp�Bq�Bq�Bq�Bt�Bu�Bu�Bu�Bv�By�B{�B}�B� B�B�B�B�B� B�B�B�B�B�B�+B�%B�B�B� B}�B}�B}�B}�B}�B|�B{�B|�B|�B~�B�B�B�B�B�+B�7B�DB�PB�VB�\B�hB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�'B�3B�LB�^B�^B�^B�^B�^B�jB�wB��BĜB��B��B�BB�yB�yB�yB�B�B��B��B��B��B��B��B��B	B	B	B	B		7B	PB	bB	oB	oB	{B	�B	�B	�B	 �B	"�B	$�B	'�B	,B	0!B	49B	7LB	<jB	?}B	@�B	B�B	C�B	E�B	F�B	I�B	N�B	P�B	P�B	S�B	W
B	YB	^5B	aHB	dZB	gmB	l�B	n�B	n�B	o�B	p�B	s�B	v�B	y�B	{�B	}�B	�B	�B	�B	�B	�B	�+B	�+B	�+B	�1B	�=B	�PB	�VB	�VB	�VB	�\B	�bB	�hB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�3B	�?B	�LB	�RB	�XB	�dB	�}B	B	ŢB	ƨB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�)B	�#B	�#B	�#B	�/B	�5B	�)B	�)B	�NB	�`B	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
+B
+B
1B
	7B

=B
	7B
	7B

=B
DB
DB
DB
JB
JB
JB
VB
\B
VB
VB
\B
\B
\B
bB
bB
hB
hB
bB
hB
�B
�B
!�B
'�B
0!B
6FB
;dB
E�B
I�B
M�B
S�B
[#B
cTB
hsB
m�B
q�B
t�B
w�B
z�B
}�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
�fB
�aB
�aB
�\B
�_B
�jB
�B
��B
��B�B�B�B�BC�Bl�B��B��B�B�WB��B�B(�B?�Bj�B~�B��BɾB��B�B�0B�BxB,B<uB=yBA�BH�BF�BJ�BS�BR�BT BUBXB[+BQ�BL�BM�BM�BH�B?�B0(B#�B�B�B�B^B+B��B��B�B��B��B��B�gB�NB��B��Bu�B^;BWBP�B5EB!�B�B��B�EB��B��Bu�B]4BP�B`IBw�Bz�Bq�BaNBJ�B;iB/%B&�B�BB
��B
��B
�B
�B
�0B
�B
��B
�#B
��B
�B
�@B
gzB
F�B
F�B
7UB
vB
.B	��B	�>B	ǽB	�8B	��B	�sB	�*B	x�B	q�B	k�B	dmB	Y)B	L�B	>�B	6]B	/0B	+B	&�B	"�B	�B	�B	|B	
QB	#B��B��B�B�B�B�B�`B�CB�0B�B��BķB�}B�\B�5B�B��B��B��B��B��B��B��B�{B�hB�cB�PB�GB�;B�,B}By�Bv�Bu�Bt�Bs�Bq�Bn�Bo�Bp�Bs�Bs�Bt�Bt�Bq�Bp�Bn�Bn�Bq�Bo�Bn�Bl�Bk�Bj�Bh�Bg�Bi�Bj�Bj�Bh�Bg�Bl�Bn�Bp�Bq�Bq�Bq�Bt�Bu�Bu�Bu�Bv�By�B|B~B�B�$B�*B�*B�#B�B�'B�.B�*B�$B�6B�IB�@B�<B�6B�B~B~B~B~B~B}B|B}B}
BB�#B�(B�+B�#B�GB�QB�`B�nB�pB�xB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�(B�@B�LB�eB�yB�wB�xB�xB�xB��B��B��BķB��B��B�YB�B�B�B��B�B��B��B��B��B��B�B�B	B	%B	(B	0B		NB	hB	wB	�B	�B	�B	�B	�B	�B	 �B	"�B	$�B	(B	,B	05B	4NB	7`B	<~B	?�B	@�B	B�B	C�B	E�B	F�B	I�B	N�B	P�B	P�B	TB	WB	Y,B	^GB	a[B	dkB	g~B	l�B	n�B	n�B	o�B	p�B	s�B	v�B	y�B	{�B	~B	�B	�B	�B	�B	�*B	�;B	�=B	�;B	�DB	�OB	�aB	�gB	�gB	�hB	�mB	�sB	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�.B	�4B	�8B	�FB	�OB	�[B	�bB	�jB	�tB	��B	 B	űB	ƷB	ǼB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	� B	�B	�!B	�7B	�4B	�5B	�3B	�@B	�DB	�6B	�7B	�]B	�nB	�B	�B	�B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B
 B
 B
B
B
B
B
B
B
B
B
)B
<B
;B
>B
	EB

KB
	EB
	CB

LB
SB
SB
TB
YB
VB
WB
eB
mB
hB
dB
kB
lB
kB
pB
pB
wB
vB
qB
wB
�B
�B
!�B
'�B
0-B
6SB
;tB
E�B
I�B
M�B
TB
[/B
c^B
h~B
m�B
q�B
t�B
w�B
z�B
~B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.06 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214462016053112144620160531121446  AO  ARCAADJP                                                                    20151114031548    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20151114031548  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20151114031548  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121446  IP                  G�O�G�O�G�O�                