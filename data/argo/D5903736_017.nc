CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:05:15Z AOML 3.0 creation; 2016-05-31T19:14:27Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20140721230515  20160531121427  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  4051_7090_017                   2C  D   APEX                            5368                            041511                          846 @�]	]��1   @�]	�P@3�9Xb�dT�/��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*fD*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh�fDi  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt� Dy�fD�fD�<�D�l�D���D� D�<�D���D��fD�3D�I�D���D��fD�	�D�` DږfD��fD�3D�<�D�p D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @|(�@�{@�{A
=A?
=A_
=A
=A��A��A��A��AυA߅A�A��BBBBB'B/B7B?BGBOBWB_BgBoBwBB��HB��HB��HB��HB�{B��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB�{B��HB��HC�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RD |)D �)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D	|)D	�)D
|)D
�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D��D|)D�)D|)D�)D |)D �)D!|)D!�)D"|)D"�)D#|)D#�)D$|)D$�)D%|)D%�)D&|)D&�)D'|)D'�)D(|)D(�)D)|)D*�D*|)D*�)D+|)D+�)D,|)D,�)D-|)D-�)D.|)D.�)D/|)D/�)D0|)D0�)D1|)D1�)D2|)D2�)D3|)D3�)D4|)D4�)D5|)D5�)D6|)D6�)D7|)D7�)D8|)D8�)D9|)D9�)D:|)D:�)D;|)D;�)D<|)D<�)D=|)D=�)D>|)D>�)D?|)D?�)D@|)D@�)DA|)DA�)DB|)DB�)DC|)DC�)DD|)DD�)DE|)DE�)DF|)DF�)DG|)DG�)DH|)DH�)DI|)DI�)DJ|)DJ�)DK|)DK�)DL|)DL�)DM|)DM�)DN|)DN�)DO|)DO�)DP|)DP�)DQ|)DQ�)DR|)DR�)DS|)DS�)DT|)DT�)DU|)DU�)DV|)DV�)DW|)DW�)DX|)DX�)DY|)DY�)DZ|)DZ�)D[|)D[�)D\|)D\�)D]|)D]�)D^|)D^�)D_|)D_�)D`|)D`�)Da|)Da�)Db|)Db�)Dc|)Dc�)Dd|)Dd�)De|)De�)Df|)Df�)Dg|)Dg�)Dh��Dh�)Di|)Di�)Dj|)Dj�)Dk|)Dk�)Dl|)Dl�)Dm|)Dm�)Dn|)Dn�)Do|)Do�)Dp|)Dp�)Dq|)Dq�)Dr|)Dr�)Ds|)Ds�)Dt|)Dt�)Dy��D�zD�:�D�j�D���D�D�:�D���D��zD�GD�G�D���D��zD��D�^DڔzD��zD�GD�:�D�nD��z111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A؃A�~�A�t�A�t�A�n�A�dZA�M�A�7LA�oA��`A�;dA� �A��A��A�{A�
=A�%A�  A���A���A���A��`A��`A��HA��TAֺ^A���A��HA���A�XA�VA��TA��;Aԉ7AӶFA��A�7LAЍPA�O�Aʏ\Aɡ�A��A�ƨA�=qAǩ�A�ƨA���A���AŲ-A��TA�A���A���A�t�A��7A� �A�{A��A�JA�C�A��A���A���A��DA��A�$�A�1A���A�
=A�
=A��RA���A���A�&�A�=qA��mA�I�A�;dA�E�A���A���A�M�A�|�A�jA�-A���A�JA�r�A�"�A���A�ĜA��#A�hsA�"�A���A��A�bA��+A��9A���A�t�A���A��wA��A�l�A��A��A��A��mA��#A�S�A���A�  A�XA��A~�DAz��Ax��Av��At�yAp�yAm�Ak�PAjA�AeG�AbȴAa��A`-A_&�A^bNA]A[�AZ�yAZ�AX�AWl�ASAP�AM�^AL�\AK`BAI|�AG?}AFA�AE�TAE��ADJAB�+ABQ�ABE�AA�#A?�A=��A<z�A;�mA;hsA:bA9K�A8�A8�A7A7/A5�-A4��A2^5A.�/A-\)A,~�A*��A)��A(ĜA'
=A&{A%;dA#��A#O�A"�RA!l�A ZA�/A�FA�A��A
=A9XA
=A�\AI�A�/Ax�A�DA��A�A�At�A�AbNA��AM�A33A5?A7LA
~�A	�FA�mA�A7LA
=A��A��A�PA+A�uA �A�A (�@��@�dZ@��@�M�@��h@�1@��R@�x�@�9X@�;d@���@�1@�|�@�@�  @��T@�\@�+@䛦@�X@�
=@��@���@�(�@ۮ@�@�n�@�V@�J@ՙ�@�?}@Լj@� �@ӍP@�+@�^5@Ь@��`@�7L@Ѓ@��@Ͼw@�K�@�dZ@��y@Ο�@�5?@ͩ�@�Q�@��@�&�@�b@�dZ@š�@Å@�E�@���@��9@�o@�V@��T@��m@��#@�1@��w@��@�J@�G�@��`@�Ĝ@��j@�r�@��F@�S�@���@���@�+@���@���@��@��D@��9@��j@��u@�bN@�b@��w@��w@�9X@���@���@��j@�j@�ƨ@��
@��F@�33@�E�@��@���@�dZ@��@��H@�=q@��@��#@��#@��-@���@�Q�@� �@��@���@�t�@�S�@�ȴ@�ff@�5?@�$�@��#@���@�hs@�7L@��@���@���@�1'@�(�@��@��w@���@�;d@�@�n�@�J@�@��T@�@���@��@���@��j@�r�@�I�@�  @��P@�\)@�@�~�@���@��T@���@���@�O�@��`@��@��D@��@��@�z�@�bN@� �@�  @��;@���@��@�\)@�S�@�"�@���@��y@�ȴ@���@�M�@�5?@�J@�G�@�%@���@���@�j@�bN@�(�@�1@��m@���@�C�@��@��\@�ff@�-@�{@��@��#@�p�@�?}@�?}@��@��/@���@��@�1'@��@���@�;d@�
=@��@���@��+@�v�@�v�@�$�@�x�@�hs@�O�@�/@��@���@��/@���@��j@��@���@�Q�@�(�@�b@�1@���@�C�@�o@��@��@���@�ff@�J@��-@�O�@�%@��`@���@��/@���@�Z@�1'@���@��P@�t�@�C�@��R@�~�@�n�@�V@�=q@���@��-@�O�@��@�%@���@��`@���@��@�z�@�r�@�j@�1'@�  @���@��
@��w@���@��P@�dZ@���@��j@vV@i�^@b�@X�`@N��@H��@A�#@<Z@5O�@0��@+o@'�@!��@ƨ@;d@t�@��@�m@�`111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A؃A�~�A�t�A�t�A�n�A�dZA�M�A�7LA�oA��`A�;dA� �A��A��A�{A�
=A�%A�  A���A���A���A��`A��`A��HA��TAֺ^A���A��HA���A�XA�VA��TA��;Aԉ7AӶFA��A�7LAЍPA�O�Aʏ\Aɡ�A��A�ƨA�=qAǩ�A�ƨA���A���AŲ-A��TA�A���A���A�t�A��7A� �A�{A��A�JA�C�A��A���A���A��DA��A�$�A�1A���A�
=A�
=A��RA���A���A�&�A�=qA��mA�I�A�;dA�E�A���A���A�M�A�|�A�jA�-A���A�JA�r�A�"�A���A�ĜA��#A�hsA�"�A���A��A�bA��+A��9A���A�t�A���A��wA��A�l�A��A��A��A��mA��#A�S�A���A�  A�XA��A~�DAz��Ax��Av��At�yAp�yAm�Ak�PAjA�AeG�AbȴAa��A`-A_&�A^bNA]A[�AZ�yAZ�AX�AWl�ASAP�AM�^AL�\AK`BAI|�AG?}AFA�AE�TAE��ADJAB�+ABQ�ABE�AA�#A?�A=��A<z�A;�mA;hsA:bA9K�A8�A8�A7A7/A5�-A4��A2^5A.�/A-\)A,~�A*��A)��A(ĜA'
=A&{A%;dA#��A#O�A"�RA!l�A ZA�/A�FA�A��A
=A9XA
=A�\AI�A�/Ax�A�DA��A�A�At�A�AbNA��AM�A33A5?A7LA
~�A	�FA�mA�A7LA
=A��A��A�PA+A�uA �A�A (�@��@�dZ@��@�M�@��h@�1@��R@�x�@�9X@�;d@���@�1@�|�@�@�  @��T@�\@�+@䛦@�X@�
=@��@���@�(�@ۮ@�@�n�@�V@�J@ՙ�@�?}@Լj@� �@ӍP@�+@�^5@Ь@��`@�7L@Ѓ@��@Ͼw@�K�@�dZ@��y@Ο�@�5?@ͩ�@�Q�@��@�&�@�b@�dZ@š�@Å@�E�@���@��9@�o@�V@��T@��m@��#@�1@��w@��@�J@�G�@��`@�Ĝ@��j@�r�@��F@�S�@���@���@�+@���@���@��@��D@��9@��j@��u@�bN@�b@��w@��w@�9X@���@���@��j@�j@�ƨ@��
@��F@�33@�E�@��@���@�dZ@��@��H@�=q@��@��#@��#@��-@���@�Q�@� �@��@���@�t�@�S�@�ȴ@�ff@�5?@�$�@��#@���@�hs@�7L@��@���@���@�1'@�(�@��@��w@���@�;d@�@�n�@�J@�@��T@�@���@��@���@��j@�r�@�I�@�  @��P@�\)@�@�~�@���@��T@���@���@�O�@��`@��@��D@��@��@�z�@�bN@� �@�  @��;@���@��@�\)@�S�@�"�@���@��y@�ȴ@���@�M�@�5?@�J@�G�@�%@���@���@�j@�bN@�(�@�1@��m@���@�C�@��@��\@�ff@�-@�{@��@��#@�p�@�?}@�?}@��@��/@���@��@�1'@��@���@�;d@�
=@��@���@��+@�v�@�v�@�$�@�x�@�hs@�O�@�/@��@���@��/@���@��j@��@���@�Q�@�(�@�b@�1@���@�C�@�o@��@��@���@�ff@�J@��-@�O�@�%@��`@���@��/@���@�Z@�1'@���@��P@�t�@�C�@��R@�~�@�n�@�V@�=q@���@��-@�O�@��@�%@���@��`@���@��@�z�@�r�@�j@�1'@�  @���@��
@��w@���@��P@�dZ@���@��j@vV@i�^@b�@X�`@N��@H��@A�#@<Z@5O�@0��@+o@'�@!��@ƨ@;d@t�@��@�m@�`111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�fB�fB�fB�fB�mB�fB�fB�fB�mB�ZB�)B�#B�)B�)B�/B�/B�5B�5B�;B�HBB�B�B#�B%�B'�B1'B;dBF�BF�BF�BJ�BJ�BI�BF�BA�B<jB2-B!�BuBoBVBDB	7B+B%B+B+B1B%BBB
=B	7B1BbB$�B1'B5?B:^B=qB<jB;dB9XB5?B/B)�B&�B"�B�BDB��B�XB{�Bp�Bs�Bp�Bl�Bs�B_;BN�BA�B0!B�BB�)BɺB�LB��By�BZBM�BM�BJ�B>wB49B!�BB
�fB
��B
��B
�B
��B
�hB
�B
�B
�1B
�+B
�%B
�B
�B
�+B
v�B
l�B
dZB
VB
@�B
6FB
)�B
�B	��B	�B	�HB	�
B	�qB	�B	��B	��B	��B	��B	�bB	�=B	�B	� B	x�B	n�B	_;B	P�B	E�B	@�B	:^B	33B	+B	&�B	$�B	!�B	�B	�B	�B	�B	hB	DB	B	B��B��B��B�B�B�B�B�sB�ZB�;B�
B��B��BǮBB��B�jB�LB�3B�'B�B�B�B��B��B��B��B��B��B��B��B�oB�VB�DB�B�B�B~�B}�B}�B|�By�Bw�By�Bv�Bt�Bs�Br�Bp�Bo�Bn�Bn�Bo�Bo�Bn�Bk�BffBe`BffBffBgmBffBffBffBe`BdZBcTBbNBbNBbNBaHB`BB_;B_;B_;B]/BYBS�BO�BN�BL�BL�BM�BM�BN�BN�BN�BO�BN�BN�BR�BR�BS�BT�BVBW
BW
B[#BbNBffBp�Bt�Bt�Bu�By�B|�B}�B�B�%B�7B�DB�+B�PB�{B��B�hB�bB�\B�VB�JB�JB�hB��B�hB�bB�VB�VB�\B�{B��B��B��B��B��B��B��B�XB�wB��BĜBɺB��B��B��B�B�B�B�5B�TB�fB�B��B��B��B��B��B	B	B	%B		7B	PB	bB	�B	�B	�B	!�B	"�B	$�B	+B	+B	5?B	9XB	:^B	<jB	?}B	B�B	C�B	H�B	K�B	L�B	N�B	P�B	R�B	T�B	VB	YB	ZB	\)B	_;B	aHB	cTB	ffB	gmB	iyB	iyB	iyB	jB	jB	k�B	k�B	l�B	s�B	v�B	w�B	y�B	{�B	� B	�B	�B	�B	�1B	�JB	�PB	�VB	�\B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�-B	�3B	�3B	�9B	�9B	�?B	�FB	�RB	�^B	�qB	�}B	��B	B	ĜB	ĜB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�#B	�B	�#B	�;B	�;B	�;B	�BB	�HB	�HB	�HB	�NB	�NB	�NB	�NB	�ZB	�`B	�`B	�`B	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
B

=B
�B
 �B
%�B
.B
2-B
8RB
>wB
E�B
K�B
P�B
S�B
ZB
^5B
bNB
iyB
k�B
o�B
s�B
y�B
|�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�nB�kB�nB�kB�vB�mB�mB�mB�uB�aB�.B�'B�.B�.B�7B�:B�;B�;B�EB�NBB�B�B#�B%�B'�B10B;kBF�BF�BF�BJ�BJ�BI�BF�BA�B<qB23B!�B{ByB`BKB	9B3B,B2B2B;B-B)B%B
CB	:B9BlB$�B12B5HB:fB=wB<qB;lB9dB5GB/#B*B&�B"�B�BOB��B�`B{�Bp�Bs�Bp�Bl�Bs�B_ABN�BA�B0%B�BB�/BɿB�RB��By�BZ$BM�BM�BJ�B>}B4?B!�B$B
�mB
�B
��B
�B
��B
�vB
�B
�B
�;B
�6B
�/B
�*B
�$B
�7B
v�B
l�B
dbB
VB
@�B
6RB
*B
�B	�B	�B	�WB	�B	��B	�+B	� B	��B	��B	��B	�tB	�OB	�1B	�B	x�B	n�B	_QB	P�B	E�B	@�B	:sB	3HB	+B	&�B	$�B	!�B	�B	�B	�B	�B	B	XB	4B	B�B��B��B��B�B��B�B�B�uB�VB�!B��B��B��BªB��B��B�eB�LB�AB�7B�'B�B�B��B��B��B��B��B��B��B��B�sB�bB�6B�'B�%BB~B~B}	By�Bw�By�Bv�Bt�Bs�Br�Bp�Bo�Bn�Bn�Bo�Bo�Bn�Bk�Bf�Be~Bf�Bf�Bg�Bf�Bf�Bf�Be�BdwBcqBblBbkBbnBajB``B_XB_YB_YB]NBY4BTBO�BN�BL�BL�BM�BM�BN�BN�BN�BP BN�BN�BSBSBTBUBV!BW*BW'B[BBbkBf�Bp�Bt�Bt�Bu�By�B}B~B�$B�@B�SB�aB�IB�kB��B��B��B�}B�xB�rB�fB�eB��B��B��B��B�pB�qB�zB��B��B��B��B��B��B��B�B�pB��B��BĵB��B��B��B�B�B� B�7B�OB�mB�{B��B��B��B��B�B�B	B	(B	;B		LB	hB	yB	�B	�B	�B	!�B	"�B	$�B	+B	+B	5SB	9kB	:rB	<B	?�B	B�B	C�B	H�B	K�B	L�B	N�B	P�B	SB	UB	VB	Y*B	Z2B	\>B	_OB	a[B	chB	fyB	gB	i�B	i�B	i�B	j�B	j�B	k�B	k�B	l�B	s�B	v�B	w�B	y�B	{�B	�B	�B	�#B	�1B	�CB	�\B	�aB	�hB	�lB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�0B	�<B	�FB	�EB	�HB	�HB	�MB	�TB	�`B	�nB	��B	��B	��B	¡B	ĮB	īB	ǿB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�$B	�'B	�-B	�3B	�.B	�3B	�JB	�KB	�LB	�QB	�YB	�WB	�YB	�]B	�]B	�[B	�]B	�gB	�oB	�oB	�mB	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	� B	� B	�B	�B
 B
 B
 B
B
B
B
#B
B
 B
(B
(B
,B

KB
�B
 �B
%�B
.#B
2<B
8_B
>�B
E�B
K�B
P�B
TB
Z)B
^BB
bYB
i�B
k�B
o�B
s�B
y�B
|�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.06 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214272016053112142720160531121427  AO  ARCAADJP                                                                    20140721230515    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230515  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230515  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121427  IP                  G�O�G�O�G�O�                