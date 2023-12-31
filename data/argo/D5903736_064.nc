CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:05:40Z AOML 3.0 creation; 2016-05-31T19:14:35Z UW 3.1 conversion     
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
_FillValue                  0  �|   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �(   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �8   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �<   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �L   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �P   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �T   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �XArgo profile    3.1 1.2 19500101000000  20140721230540  20160531121435  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               @A   AO  4051_7090_064                   2C  D   APEX                            5368                            041511                          846 @��j�π 1   @��k�{@@4hr� Ĝ�e� ě�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    @A   B   B   @9��@�  @���A   A   A@  A`  A���A�  A�  A�  A�  A�  A�  A�  B   B  B  B��B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$y�D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDyy�D��D�<�D�y�D��fD�3D�9�D��3D��3D�	�D�@ D�� D��fD�fD�S3DچfD��3D��fD�@ D�vfD���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @5@|(�@��H@�{A
=A?
=A_
=A�Q�A��A��A��A��AυA߅A�A��BBB\)BB'B/B7B?BGBOBWB_BgBoBwBB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB�{B��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HC�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RD |)D �)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D	|)D	�)D
|)D
�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D |)D �)D!|)D!�)D"|)D"�)D#|)D#�)D$u�D$�)D%|)D%�)D&|)D&�)D'|)D'�)D(|)D(�)D)|)D)�)D*|)D*�)D+|)D+�)D,|)D,�)D-|)D-�)D.|)D.�)D/|)D/�)D0|)D0�)D1|)D1�)D2|)D2�)D3|)D3�)D4|)D4�)D5|)D5�)D6|)D6�)D7|)D7�)D8|)D8�)D9|)D9�)D:|)D:�)D;|)D;�)D<|)D<�)D=|)D=�)D>|)D>�)D?|)D?�)D@|)D@�)DA|)DA�)DB|)DB�)DC|)DC�)DD|)DD�)DE|)DE�)DF|)DF�)DG|)DG�)DH|)DH�)DI|)DI�)DJ|)DJ�)DK|)DK�)DL|)DL�)DM|)DM�)DN|)DN�)DO|)DO�)DP|)DP�)DQ|)DQ�)DR|)DR�)DS|)DS�)DT|)DT�)DU|)DU�)DV|)DV�)DW|)DW�)DX|)DX�)DY|)DY�)DZ|)DZ�)D[|)D[�)D\|)D\�)D]|)D]�)D^|)D^�)D_|)D_�)D`|)D`�)Da|)Da�)Db|)Db�)Dc|)Dc�)Dd|)Dd�)De|)De�)Df|)Df�)Dg|)Dg�)Dh|)Dh�)Di|)Di�)Dj|)Dj�)Dk|)Dk�)Dl|)Dl�)Dm|)Dm�)Dn|)Dn�)Do|)Do�)Dp|)Dp�)Dq|)Dq�)Dr|)Dr�)Ds|)Ds�)Dt|)DtDyu�D�
�D�:�D�w�D��zD�GD�7�D��GD��GD��D�>D�~D��zD�zD�QGDڄzD��GD��zD�>D�tzD���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A˲-A˶FA˺^A˺^A˸RA˺^A˺^A˺^A˺^A˼jA˼jA˾wA���A���A���A�A�A�ĜA�ĜA�ƨA�ƨA�ƨA�ƨA�ȴA�ȴA�ƨA˥�A˅A�VA��Aʩ�A�~�A�bNA�I�A�33A�AɮA�^5A�C�A�?}A�9XA�"�A���A���A���Aĩ�A�M�A���A��FA�5?A��;A���A�33A�~�A��;A�p�A���A�{A�z�A��A�?}A���A�jA�jA���A��A��-A�O�A���A���A��uA�+A���A���A��-A� �A��#A���A�?}A�ȴA�ffA���A��A���A��#A�A���A�G�A��A��A���A��DA��A���A�l�A���A�&�A�VA�v�A���A�C�A���A���A�A���A�{A���A��\A��A���A��A�S�A��HA��7A��9A��A�  A�=qA�S�A�v�A�9XA���A��TA~�jAzĜAx��Aw��As�mAq�hApr�Aot�Aln�Ai�hAf��Ae��Ad�/Ab�/A_dZAY��AW�-AVM�AU�PAT�!AT  AS�hAS
=ARVAR1AQS�APE�AN�9AM�ALȴAJ�AG�PAF(�AC�hAB��ABffABI�ABJAA�#AA�wAA�PA@�DA=XA<1'A;�A;��A;��A;��A;��A;S�A9dZA6�yA6I�A6A�A5�A5l�A4��A4 �A1S�A/��A,�A( �A'�7A';dA&��A&VA%�mA$��A#dZA"�9A"ZA!ƨA!oA �Av�A��A�hA�Ar�AE�A-A�A��A�`AbAffA/AA�A?}A$�A�A��A��A��A
��A
Q�A
�A
1A	+A-A�wAZA��AO�Az�A�A�A�uA7LA =q@�p�@�Q�@�9X@��w@�l�@��+@��@�Z@�Q�@�  @��+@�(�@�P@�C�@�-@���@��@�  @���@�@���@�C�@�+@�ff@�/@�^5@ݙ�@�&�@�b@١�@ׅ@�J@�"�@�bN@�\)@���@�Ĝ@�v�@�@�V@�Z@ǍP@��@���@�X@�%@��`@Ĵ9@�z�@�S�@�@�E�@��@��T@���@�G�@���@�Z@��;@�l�@�v�@��T@��h@�V@��@���@��@��D@�(�@�b@���@��!@��@���@�33@��\@�^5@���@��7@�7L@��9@��@���@���@���@���@�@���@�r�@�@��^@�X@�?}@��`@�z�@�A�@�33@��y@�ȴ@��+@�v�@�ff@�M�@�{@���@���@�x�@�r�@�ƨ@���@���@�|�@�@���@��@��T@��h@�X@�/@��/@��m@��F@�t�@�
=@���@�~�@�=q@�$�@�=q@�5?@��@��^@�hs@�`B@�`B@�`B@�`B@�X@�V@�%@��/@�A�@��;@��P@��@��+@�5?@�7L@���@���@��j@���@��@�j@�Z@�9X@�b@��
@�;d@�@��y@��\@�v�@�v�@�ff@�ff@�ff@�ff@�~�@��\@��\@��+@�$�@�J@���@��@���@��-@��7@�hs@�7L@�&�@���@��j@���@�1'@�9X@� �@�b@��@��
@���@��@��P@���@���@�l�@�dZ@�;d@���@�~�@�5?@���@��h@�`B@�X@�?}@�7L@�`B@�X@�?}@�7L@�&�@��@�V@�Ĝ@�b@��m@���@��F@��F@��F@��w@��F@��F@��F@��F@��@���@���@���@���@��P@�t�@�C�@�+@�o@�o@���@���@��+@�~�@�V@��@��#@��7@�hs@�/@��@�r�@�Q�@�  @��
@��F@��@�\)@���@��@�I�@���@t��@j�@[��@T��@P�@J�\@G
=@>��@8�u@-�-@)x�@l�@�^@/@�#@��@	7L@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 A˲-A˶FA˺^A˺^A˸RA˺^A˺^A˺^A˺^A˼jA˼jA˾wA���A���A���A�A�A�ĜA�ĜA�ƨA�ƨA�ƨA�ƨA�ȴA�ȴA�ƨA˥�A˅A�VA��Aʩ�A�~�A�bNA�I�A�33A�AɮA�^5A�C�A�?}A�9XA�"�A���A���A���Aĩ�A�M�A���A��FA�5?A��;A���A�33A�~�A��;A�p�A���A�{A�z�A��A�?}A���A�jA�jA���A��A��-A�O�A���A���A��uA�+A���A���A��-A� �A��#A���A�?}A�ȴA�ffA���A��A���A��#A�A���A�G�A��A��A���A��DA��A���A�l�A���A�&�A�VA�v�A���A�C�A���A���A�A���A�{A���A��\A��A���A��A�S�A��HA��7A��9A��A�  A�=qA�S�A�v�A�9XA���A��TA~�jAzĜAx��Aw��As�mAq�hApr�Aot�Aln�Ai�hAf��Ae��Ad�/Ab�/A_dZAY��AW�-AVM�AU�PAT�!AT  AS�hAS
=ARVAR1AQS�APE�AN�9AM�ALȴAJ�AG�PAF(�AC�hAB��ABffABI�ABJAA�#AA�wAA�PA@�DA=XA<1'A;�A;��A;��A;��A;��A;S�A9dZA6�yA6I�A6A�A5�A5l�A4��A4 �A1S�A/��A,�A( �A'�7A';dA&��A&VA%�mA$��A#dZA"�9A"ZA!ƨA!oA �Av�A��A�hA�Ar�AE�A-A�A��A�`AbAffA/AA�A?}A$�A�A��A��A��A
��A
Q�A
�A
1A	+A-A�wAZA��AO�Az�A�A�A�uA7LA =q@�p�@�Q�@�9X@��w@�l�@��+@��@�Z@�Q�@�  @��+@�(�@�P@�C�@�-@���@��@�  @���@�@���@�C�@�+@�ff@�/@�^5@ݙ�@�&�@�b@١�@ׅ@�J@�"�@�bN@�\)@���@�Ĝ@�v�@�@�V@�Z@ǍP@��@���@�X@�%@��`@Ĵ9@�z�@�S�@�@�E�@��@��T@���@�G�@���@�Z@��;@�l�@�v�@��T@��h@�V@��@���@��@��D@�(�@�b@���@��!@��@���@�33@��\@�^5@���@��7@�7L@��9@��@���@���@���@���@�@���@�r�@�@��^@�X@�?}@��`@�z�@�A�@�33@��y@�ȴ@��+@�v�@�ff@�M�@�{@���@���@�x�@�r�@�ƨ@���@���@�|�@�@���@��@��T@��h@�X@�/@��/@��m@��F@�t�@�
=@���@�~�@�=q@�$�@�=q@�5?@��@��^@�hs@�`B@�`B@�`B@�`B@�X@�V@�%@��/@�A�@��;@��P@��@��+@�5?@�7L@���@���@��j@���@��@�j@�Z@�9X@�b@��
@�;d@�@��y@��\@�v�@�v�@�ff@�ff@�ff@�ff@�~�@��\@��\@��+@�$�@�J@���@��@���@��-@��7@�hs@�7L@�&�@���@��j@���@�1'@�9X@� �@�b@��@��
@���@��@��P@���@���@�l�@�dZ@�;d@���@�~�@�5?@���@��h@�`B@�X@�?}@�7L@�`B@�X@�?}@�7L@�&�@��@�V@�Ĝ@�b@��m@���@��F@��F@��F@��w@��F@��F@��F@��F@��@���@���@���@���@��P@�t�@�C�@�+@�o@�o@���@���@��+@�~�@�V@��@��#@��7@�hs@�/@��@�r�@�Q�@�  @��
@��F@��@�\)G�O�@��@�I�@���@t��@j�@[��@T��@P�@J�\@G
=@>��@8�u@-�-@)x�@l�@�^@/@�#@��@	7L@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�TB�TB�TB�TB�TB�TB�TB�TB�TB�TB�TB�TB�TB�TB�TB�TB�TB�TB�TB�TB�TB�TB�TB�TB�TB�TB�NB�NB�HB�TB�B��B%BJB\B�B�B�B�B�B"�B"�B�BDB��B�5B��BÖB��B�jB�}B��B�RB�B�7Bp�BbNB]/BW
BJ�B8RB-B%�B �B�BhBVBDB	7B+BBB��B�B�B��B��BǮBÖB�jB�FB�B��B�uB�hB�\B�JB�+B� Bx�Bl�BVB@�B8RB49B+B�B�BoB1B��B�/B�dB�'B��B�hB�DB�+B{�BbNB8RB �B�B\B
��B
�HB
�B
��B
�dB
��B
�oB
~�B
e`B
Q�B
9XB
-B
#�B
PB	��B	��B	�B	�5B	��B	�}B	�LB	�!B	��B	�\B	o�B	gmB	`BB	^5B	\)B	]/B	[#B	XB	W
B	T�B	R�B	O�B	L�B	G�B	D�B	=qB	6FB	1'B	'�B	%�B	$�B	$�B	#�B	"�B	!�B	�B	�B	bB	JB	DB		7B	1B	+B	%B	B��B�B�B�B�B�B�fB�BB�B��BÖB�?B�-B�'B�!B�B�B��B��B��B��B��B��B��B�{B�hB�\B�VB�PB�PB�JB�DB�7B�B�B|�By�Bw�Bt�Br�Bo�Bm�Bk�BjBiyBiyBiyBhsBffBe`BdZBbNBbNBaHB`BB_;B^5B]/B\)B[#B[#B\)B\)B\)B\)B\)B\)B]/B]/B\)B\)B]/B[#B]/B\)B\)B`BBaHBbNBbNBdZBcTBbNB`BB^5BaHBaHB`BB`BBbNBdZBffBl�Bs�Bt�Bw�Bw�B|�B�B� B�B�+B�JB��B��B�B�B�B�B�'B�LB�XB�^B�dB�qB��BŢBǮB��B��B��B��B�B�
B�B�#B�HB�mB�yB�sB�yB�B��B��B��B��B��B��B	  B	B	B	%B	
=B	DB	VB	bB	oB	�B	�B	�B	 �B	!�B	"�B	#�B	$�B	$�B	+B	,B	-B	.B	.B	.B	/B	1'B	2-B	33B	49B	;dB	B�B	C�B	C�B	D�B	J�B	P�B	W
B	YB	\)B	]/B	^5B	_;B	hsB	iyB	iyB	jB	jB	m�B	r�B	{�B	}�B	~�B	� B	�B	�1B	�1B	�1B	�7B	�7B	�7B	�1B	�1B	�1B	�=B	�JB	�VB	�bB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�'B	�9B	�?B	�LB	�RB	�RB	�XB	�XB	�XB	�^B	�^B	�dB	�jB	�dB	�dB	�^B	�^B	�qB	�wB	��B	��B	B	ÖB	ŢB	ƨB	ǮB	ȴB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�)B	�BB	�BB	�TB	�ZB	�`B	�`B	�`B	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B
%B	��B
+B
\B
�B
#�B
0!B
5?B
8RB
=qB
A�B
G�B
M�B
XB
\)B
e`B
hsB
m�B
p�B
v�B
x�B
|�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 B�aB�bB�bB�bB�bB�`B�dB�bB�bB�bB�bB�bB�dB�dB�bB�dB�`B�bB�bB�dB�bB�`B�bB�bB�dB�dB�ZB�[B�SB�_B�B��B3BYBiB�B�B�B�B�B"�B"�B�BPB��B�HB��BáB��B�tB��B��B�^B�B�?Bp�BbWB];BWBJ�B8[B-B%�B �B�BjB`BKB	=B1B(BB��B�B�B��B��BǴBÝB�rB�LB�B��B�zB�mB�eB�TB�1B�
Bx�Bl�BV	B@�B8XB4<B+	B�B�BrB7B��B�4B�jB�-B��B�nB�IB�2B{�BbSB8XB �B�BbB
��B
�QB
�B
��B
�lB
��B
�yB
B
enB
Q�B
9cB
-B
#�B
^B	�B	��B	�B	�EB	��B	��B	�]B	�0B	��B	�lB	o�B	gB	`TB	^IB	\=B	]AB	[7B	X#B	WB	UB	SB	O�B	L�B	G�B	D�B	=�B	6]B	1:B	(B	%�B	$�B	$�B	#�B	"�B	!�B	�B	�B	xB	_B	[B		PB	JB	CB	<B	$B��B�B�B�B��B�B�|B�ZB�)B��BðB�]B�JB�AB�;B�.B�&B�B��B��B��B��B��B��B��B��B�yB�pB�lB�nB�fB�aB�VB�;B�$B}By�Bw�Bt�Br�Bo�Bm�Bk�Bj�Bi�Bi�Bi�Bh�Bf�BeBdvBbjBbmBahB`cB_ZB^SB]LB\JB[CB[CB\GB\IB\IB\HB\HB\HB]LB]MB\HB\FB]MB[CB]LB\HB\FB``BafBblBbjBduBcpBblB``B^RBadBagB``B``BbmBdvBf�Bl�Bs�Bt�Bw�Bw�B}
B�#B�B�/B�GB�fB��B�B�B�)B�-B�/B�AB�hB�qB�vB�|B��B��BŻB��B��B��B�B�B�B�#B�*B�<B�`B�B�B�B�B�B��B��B��B� B�B�B	 B	B	2B	:B	
TB	YB	nB	xB	�B	�B	�B	�B	 �B	!�B	"�B	#�B	$�B	$�B	+B	,B	-$B	.+B	.*B	.*B	/1B	1<B	2@B	3GB	4PB	;yB	B�B	C�B	C�B	D�B	J�B	P�B	WB	Y,B	\<B	]@B	^JB	_NB	h�B	i�B	i�B	j�B	j�B	m�B	r�B	{�B	~B	B	�B	�/B	�BB	�BB	�DB	�EB	�GB	�HB	�CB	�DB	�AB	�NB	�[B	�iB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�	B	�B	�B	�B	�B	�B	�$B	�,B	�7B	�GB	�OB	�\B	�aB	�bB	�hB	�hB	�kB	�oB	�oB	�tB	�{B	�xB	�wB	�pB	�qB	��B	��B	��B	��B	 B	æB	űB	ƹB	ǿB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�7B	�SB	�TB	�cB	�hB	�nB	�oB	�oB	�|B	�B	�B	��B	�B	��B	��B	�B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��G�O�B	��B
:B
kB
�B
#�B
0-B
5LB
8`B
=�B
A�B
G�B
M�B
XB
\4B
emB
h}B
m�B
p�B
v�B
x�B
|�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.06 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214352016053112143520160531121435  AO  ARCAADJP                                                                    20140721230540    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230540  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230540  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121435  IP                  G�O�G�O�G�O�                