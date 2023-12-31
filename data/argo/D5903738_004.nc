CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:06:36Z AOML 3.0 creation; 2016-05-31T21:48:57Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20140721230636  20160531144857  5903738 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  4053_7107_004                   2C  D   APEX                            5370                            041511                          846 @�4��1   @�4�~�z@8Tz�G��c'��v�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy� D�)�D�I�D���D��3D��D�I�D�� D��3D��D�VfD�� D��3D� D�VfDڌ�D�3D�	�D�6fD�ffD��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�G�@�{@�{A
=A?
=A_
=A
=A��A��A��A��AυA߅A�A��BBBBB'B/B7B?BGBOBWB_BgBoBwBB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HC�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RD |)D �)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D	|)D	�)D
|)D
�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D |)D �)D!|)D!�)D"|)D"�)D#|)D#�)D$|)D$�)D%|)D%�)D&|)D&�)D'|)D'�)D(|)D(�)D)|)D)�)D*|)D*�)D+|)D+�)D,|)D,�)D-|)D-�)D.|)D.�)D/|)D/�)D0|)D0�)D1|)D1�)D2|)D2�)D3|)D3�)D4|)D4�)D5|)D5�)D6|)D6�)D7|)D7�)D8|)D8�)D9|)D9�)D:|)D:�)D;|)D;�)D<|)D<�)D=|)D=�)D>|)D>�)D?|)D?�)D@|)D@�)DA|)DA�)DB|)DB�)DC|)DC�)DD|)DD�)DE|)DE�)DF|)DF�)DG|)DG�)DH|)DH�)DI|)DI�)DJ|)DJ�)DK|)DK�)DL|)DL�)DM|)DM�)DN|)DN�)DO|)DO�)DP|)DP�)DQ|)DQ�)DR|)DR�)DS|)DS�)DT|)DT�)DU|)DU�)DV|)DV�)DW|)DW�)DX|)DX�)DY|)DY�)DZ|)DZ�)D[|)D[�)D\|)D\�)D]|)D]�)D^|)D^�)D_|)D_�)D`|)D`�)Da|)Da�)Db|)Db�)Dc|)Dc�)Dd|)Dd�)De|)De�)Df|)Df�)Dg|)Dg�)Dh|)Dh�)Di|)Di�)Dj|)Dj�)Dk|)Dk�)Dl|)Dl�)Dm|)Dm�)Dn|)Dn�)Do|)Do�)Dp|)Dp�)Dq|)Dq�)Dr|)Dr�)Ds|)Ds�)Dt|)Dt��Dy�)D�'�D�G�D���D��GD��D�G�D�~D��GD��D�TzD�~D��GD�D�TzDڊ�D�GD��D�4zD�dzD��G111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�K�A�z�A�;dA� �A�%A���A���A���A��A��yA��TA��/A��A��
A���A���A���A�A��wA��wA��wA��jA��-A���A���A�~�A�Q�A�A�A�{A��-A�-A���A���A�Q�A���A��A��/A��A�A��-A��;A�dZA�l�A�^5A�"�A�x�A�bA�n�A���A���A��A��TA���A���A�|�A�hsA�I�A�VA��A���A��`A��+A�"�A��9A�|�A�I�A�\)A�bA�"�A��/A���A��;A��A�%A�z�A�^5A�?}A�A�=qA�&�A��A�ȴA�
=A�`BA���A�A�dZA��FA���A���A���A�?}A���A���A�VA��;A��A���A�ƨA�+A�"�A��`A���A���A� �A�ffA���A�O�A�=qA�/A�E�A}hsA{t�Az�AxȴAx  Av�Aux�Ar��An��Am/Ai��Ag;dAc�^A`M�A^Q�A]t�A\�A[��A[AY�AU�AS��ASdZAS�hAQ7LAN1'AL�\AK�AI��AFz�AC�#AA�#AA��AA�A@��A@��A@�9A@�A>�A<n�A;�A;"�A:��A:=qA81'A7��A7`BA5��A4ĜA3��A1��A/?}A.�uA-�A+��A*�A*�9A*r�A)A(v�A&��A$E�A#S�A"�A!�A ��A�wA�+A �A&�AƨAȴA��A�PAG�A�A��AE�A��AhsA�+A1A��A�uA�wAdZA�\A�Az�AbA��A��A��A$�A
ȴA	��A	?}A�A\)AI�A��AoA��A��AbNAAC�A��A�FA"�A (�@�o@��R@�@�Q�@�$�@�(�@�K�@�@���@��y@���@�&�@�dZ@�h@�?}@�r�@�F@��@�ff@���@�b@��@�$�@�p�@�@�"�@��@�7L@�M�@��@���@�  @ۅ@�n�@�j@թ�@Ӯ@ҧ�@љ�@� �@�p�@�9X@���@�v�@�$�@���@Ɂ@ȣ�@��@�J@���@Ĵ9@Ĵ9@Ĭ@�b@���@�1@�ƨ@Å@���@�=q@�=q@���@���@���@��u@��D@�r�@�j@�&�@�@�@���@�V@��@�ȴ@���@��@���@�dZ@�-@�?}@���@�%@��@��\@���@��D@�\)@��@���@��`@�b@�
=@�~�@�V@���@��/@��@���@�\)@�M�@��@��@��@��@��R@��\@�~�@�^5@��T@��-@���@��h@�n�@���@���@���@���@��^@��@�?}@��#@��@�Ĝ@�?}@�O�@�p�@�X@���@�1@���@�&�@�/@��9@��
@�ff@��@�A�@��@�dZ@�@�O�@��D@���@���@�dZ@��@��H@���@��!@�ff@�=q@�J@��#@���@���@�G�@��@��@���@��j@��u@�Z@�(�@�1@�  @�1@�I�@�bN@�j@���@�?}@�O�@�&�@��j@��9@�A�@��P@�K�@���@��\@�=q@�n�@��@�S�@�;d@�\)@�33@��@�@�@��@���@��R@�^5@���@��7@��@��u@�r�@�Q�@�  @�t�@�;d@�33@�33@�;d@�C�@�C�@�+@��@�o@�@��@��H@���@��!@�n�@�$�@��#@�@���@��7@�p�@�O�@�hs@��7@���@�G�@�X@���@��#@��#@���@�p�@��@���@�(�@�  @��@���@���@��F@�S�@�
=@�ȴ@��y@�ȴ@���@�V@�E�@�-@��@���@��@��u@�Q�@�1@��;@�ƨ@��F@��F@��
@��@�t�@�;d@�x�@zM�@q��@l��@g�w@`�`@Y��@R�@J��@A�7@8 �@1hs@,�@'��@#S�@�D@�9@9X@ �@j@�P111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�K�A�z�A�;dA� �A�%A���A���A���A��A��yA��TA��/A��A��
A���A���A���A�A��wA��wA��wA��jA��-A���A���A�~�A�Q�A�A�A�{A��-A�-A���A���A�Q�A���A��A��/A��A�A��-A��;A�dZA�l�A�^5A�"�A�x�A�bA�n�A���A���A��A��TA���A���A�|�A�hsA�I�A�VA��A���A��`A��+A�"�A��9A�|�A�I�A�\)A�bA�"�A��/A���A��;A��A�%A�z�A�^5A�?}A�A�=qA�&�A��A�ȴA�
=A�`BA���A�A�dZA��FA���A���A���A�?}A���A���A�VA��;A��A���A�ƨA�+A�"�A��`A���A���A� �A�ffA���A�O�A�=qA�/A�E�A}hsA{t�Az�AxȴAx  Av�Aux�Ar��An��Am/Ai��Ag;dAc�^A`M�A^Q�A]t�A\�A[��A[AY�AU�AS��ASdZAS�hAQ7LAN1'AL�\AK�AI��AFz�AC�#AA�#AA��AA�A@��A@��A@�9A@�A>�A<n�A;�A;"�A:��A:=qA81'A7��A7`BA5��A4ĜA3��A1��A/?}A.�uA-�A+��A*�A*�9A*r�A)A(v�A&��A$E�A#S�A"�A!�A ��A�wA�+A �A&�AƨAȴA��A�PAG�A�A��AE�A��AhsA�+A1A��A�uA�wAdZA�\A�Az�AbA��A��A��A$�A
ȴA	��A	?}A�A\)AI�A��AoA��A��AbNAAC�A��A�FA"�A (�@�o@��R@�@�Q�@�$�@�(�@�K�@�@���@��y@���@�&�@�dZ@�h@�?}@�r�@�F@��@�ff@���@�b@��@�$�@�p�@�@�"�@��@�7L@�M�@��@���@�  @ۅ@�n�@�j@թ�@Ӯ@ҧ�@љ�@� �@�p�@�9X@���@�v�@�$�@���@Ɂ@ȣ�@��@�J@���@Ĵ9@Ĵ9@Ĭ@�b@���@�1@�ƨ@Å@���@�=q@�=q@���@���@���@��u@��D@�r�@�j@�&�@�@�@���@�V@��@�ȴ@���@��@���@�dZ@�-@�?}@���@�%@��@��\@���@��D@�\)@��@���@��`@�b@�
=@�~�@�V@���@��/@��@���@�\)@�M�@��@��@��@��@��R@��\@�~�@�^5@��T@��-@���@��h@�n�@���@���@���@���@��^@��@�?}@��#@��@�Ĝ@�?}@�O�@�p�@�X@���@�1@���@�&�@�/@��9@��
@�ff@��@�A�@��@�dZ@�@�O�@��D@���@���@�dZ@��@��H@���@��!@�ff@�=q@�J@��#@���@���@�G�@��@��@���@��j@��u@�Z@�(�@�1@�  @�1@�I�@�bN@�j@���@�?}@�O�@�&�@��j@��9@�A�@��P@�K�@���@��\@�=q@�n�@��@�S�@�;d@�\)@�33@��@�@�@��@���@��R@�^5@���@��7@��@��u@�r�@�Q�@�  @�t�@�;d@�33@�33@�;d@�C�@�C�@�+@��@�o@�@��@��H@���@��!@�n�@�$�@��#@�@���@��7@�p�@�O�@�hs@��7@���@�G�@�X@���@��#@��#@���@�p�@��@���@�(�@�  @��@���@���@��F@�S�@�
=@�ȴ@��y@�ȴ@���@�V@�E�@�-@��@���@��@��u@�Q�@�1@��;@�ƨ@��F@��F@��
@��@�t�@�;d@�x�@zM�@q��@l��@g�w@`�`@Y��@R�@J��@A�7@8 �@1hs@,�@'��@#S�@�D@�9@9X@ �@j@�P111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�qB�}B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BBBBÖBȴBɺB��B�BB�BB$�B>wBI�B`BBiyB~�B�=B�%Bs�Bl�Bp�Bm�BJ�B7LB+B�BuB�B,B8RB8RB49B1'B/B-B(�B�B	7B�B�B�fB�BB�B��B�wB��B�VBu�B`BBQ�BC�B1'B'�B$�B �B�BbBVBB��B�B�NB��BǮB�LB��B��BiyBF�B7LB'�BhB
��B
�TB
�B
�oB
�B
t�B
bNB
\)B
YB
S�B
F�B
"�B	�ZB	�)B	�B	�
B	ŢB	��B	�PB	�%B	w�B	q�B	hsB	e`B	[#B	G�B	6FB	�B	PB��B�TB�B��B��BȴB��B�RB��B��B�B�wB�}B��B��B�oB�Bk�BR�BJ�BZB`BBdZBe`BdZBdZBt�B�%B�7B�7B�=B�1B�7B�1B�%B�B�%B�B�B�B� B|�By�Bx�Bw�Bv�Bt�Bp�Bl�BgmBffBdZBcTB`BB^5B\)B[#BYBW
BVBT�BS�BS�BQ�BP�BP�BP�BO�BN�BL�BJ�BF�BC�BA�B>wB>wB<jB;dB:^B9XB7LB5?B49B33B2-B1'B2-B1'B1'B2-B33B33B33B6FB6FB8RB:^B:^B;dB;dB9XB7LB6FB1'B-B,B,B,B+B,B,B/B49B6FB7LB:^B=qBA�BA�BD�BG�BK�BL�BN�BM�BL�BK�BH�BD�BB�B@�B?}B<jB;dB6FB33B49B5?B6FB8RB:^B=qB?}BA�BB�BE�BM�BN�BN�BQ�BS�BR�BR�BW
BgmBjBm�Bq�Bs�Bp�Bu�Bz�B|�B}�B�B�B�%B�1B�bB��B��B��B��B��B��B��B��B��B��B��B�uB�oB��B�uB�B�B�B�B�B�+B�+B�=B�VB�oB�oB�oB��B��B��B��B��B��B��B�B�B�B�!B�'B�3B�jB�qB�wBƨB��B�B�B�B�#B�HB�NB�yB�B�B�B��B��B��B��B��B��B��B�B��B��B��B��B�B�B�B�B�B�B�B�B��B��B��B	B	%B	%B	1B	DB	VB	oB	uB	�B	�B	�B	�B	!�B	%�B	'�B	+B	.B	33B	7LB	9XB	?}B	B�B	D�B	J�B	O�B	R�B	VB	\)B	^5B	`BB	bNB	dZB	hsB	iyB	k�B	n�B	s�B	u�B	v�B	y�B	y�B	{�B	|�B	~�B	�B	�B	�B	�B	�%B	�%B	�1B	�7B	�VB	�\B	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�-B	�FB	�RB	�^B	�dB	�qB	��B	ÖB	ĜB	ƨB	ƨB	ƨB	ŢB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	�B	�B	�#B	�#B	�)B	�/B	�/B	�B	�B	�B	�B	�B	�B	�#B	�)B	�/B	�BB	�BB	�BB	�ZB	�sB	��B
B
PB
{B
�B
$�B
-B
49B
=qB
E�B
K�B
O�B
S�B
XB
`BB
dZB
iyB
m�B
p�B
t�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�wB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BBBBÝBȼBɿB��B�JB�B(B$�B>BI�B`NBi�BB�JB�0Bs�Bl�Bp�Bm�BJ�B7UB+B�B}B�B,B8ZB8[B4AB1/B/$B-B)B�B	=B��B�B�lB�JB�%B��B�B��B�\Bu�B`GBQ�BC�B1-B'�B$�B �B�BeBXB&B��B�B�QB��BǴB�SB��B��Bi�BF�B7SB'�BoB
��B
�\B
�B
�xB
�B
t�B
bYB
\3B
Y$B
T B
F�B
"�B	�jB	�9B	�.B	�B	ŰB	��B	�bB	�7B	w�B	q�B	h�B	erB	[4B	G�B	6ZB	�B	fB��B�mB�(B�B��B��B��B�lB�B�B�(B��B��B�
B��B��B�+Bk�BSBJ�BZ=B`aBdwBe~BdvBdxBt�B�BB�QB�TB�ZB�OB�WB�PB�BB�;B�AB�;B�1B�+B�B}	By�Bx�Bw�Bv�Bt�Bp�Bl�Bg�Bf�BdxBcsB`bB^SB\GB[CBY6BW*BV!BUBTBTBRBQBQBQBO�BN�BL�BJ�BF�BC�BA�B>�B>�B<�B;�B:B9zB7mB5_B4XB3TB2MB1GB2PB1IB1+B2NB3RB3TB3RB6gB6gB8sB:B:B;�B;�B9yB7oB6eB1HB-.B,'B,'B,'B+%B,)B,(B/=B4ZB6dB7nB:}B=�BA�BA�BD�BG�BK�BL�BN�BM�BL�BK�BH�BD�BB�B@�B?�B<�B;�B6fB3UB4XB5^B6dB8qB:�B=�B?�BA�BB�BE�BM�BN�BN�BR	BTBSBSBW)Bg�Bj�Bm�Bq�Bs�Bp�Bu�Bz�B}B~B�$B�0B�AB�MB�~B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�;B�(B�(B�.B�9B�HB�HB�ZB�pB��B��B��B��B��B��B��B��B��B��B�B�.B�5B�9B�CB�KB��B��B��BƾB�B�'B�(B�'B�;B�_B�fB�B��B��B�B��B��B��B��B�B��B��B��B��B��B��B��B��B�B�B��B�B�B�B��B��B��B�B	7B	;B	:B	IB	ZB	kB	�B	�B	�B	�B	�B	�B	!�B	%�B	(B	+B	.+B	3IB	7aB	9hB	?�B	B�B	D�B	J�B	O�B	SB	VB	\<B	^IB	`WB	b`B	dmB	h�B	i�B	k�B	n�B	s�B	u�B	v�B	y�B	y�B	{�B	|�B	
B	� B	�B	�!B	�0B	�6B	�7B	�CB	�HB	�jB	�oB	�tB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�-B	�.B	�8B	�<B	�VB	�bB	�pB	�wB	��B	��B	åB	ĭB	ƸB	ƸB	ƺB	ŲB	ƶB	��B	��B	��B	��B	�B	�B	�B	�B	� B	�$B	�2B	�3B	�9B	�?B	�=B	�*B	�,B	�+B	�-B	�.B	�,B	�3B	�8B	�=B	�QB	�RB	�QB	�iB	�B	��B
!B
_B
�B
�B
$�B
-B
4GB
=~B
E�B
K�B
O�B
TB
XB
`OB
dfB
i�B
m�B
p�B
t�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.06 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311448572016053114485720160531144857  AO  ARCAADJP                                                                    20140721230636    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230636  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230636  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531144857  IP                  G�O�G�O�G�O�                