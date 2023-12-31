CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-11-20T13:00:48Z AOML 3.0 creation; 2016-05-31T19:14:40Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20141120130048  20160531121440  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               _A   AO  4051_7090_095                   2C  D   APEX                            5368                            041511                          846 @�$�6lw�1   @�$��/�@4�bM���dlI�^5?1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    _A   A   A   @���@�  A   AffA@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bi33Bo33Bw��B�  B�  B�  B�  B�33B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX�CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DN��DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy�fD��D�S3D���D�� D�fD�@ D��fD��fD�3D�C3D��fDǶfD�	�D�I�DچfD�ٚD��D�I�D� D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@�{@�{Ap�A?
=A_
=A
=A��A��A��A��AυA߅A�A��BBBBB'B/B7B?BGBOBWB_Bh��Bn��Bw\)BB��HB��HB��HB�{B��HB��HB��HB��HB��B��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB�{B��HC�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CX
>CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC�C��RC��RC�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RD |)D �)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D	|)D	�)D
|)D
�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D |)D �)D!|)D!�)D"|)D"�)D#|)D#�)D$|)D$�)D%|)D%�)D&|)D&�)D'|)D'�)D(|)D(�)D)|)D)�)D*|)D*�)D+|)D+�)D,|)D,�)D-|)D-�)D.|)D.�)D/|)D/�)D0|)D0�)D1|)D1�)D2|)D2�)D3|)D3�)D4|)D4�)D5|)D5�)D6|)D6�)D7|)D7�)D8|)D8�)D9|)D9�)D:|)D:�)D;|)D;�)D<|)D<�)D=|)D=�)D>|)D>�)D?|)D?�)D@|)D@�)DA|)DA�)DB|)DB�)DC|)DC�)DD|)DD�)DE|)DE�)DF|)DF�)DG|)DG�)DH|)DH�)DI|)DI�)DJ|)DJ�)DK|)DK�)DL|)DL�)DM|)DM�)DN|)DN��DO|)DO�)DP|)DP�)DQ|)DQ�)DR|)DR�)DS|)DS�)DT|)DT�)DU|)DU�)DV|)DV�)DW|)DW�)DX|)DX�)DY|)DY�)DZ|)DZ�)D[|)D[�)D\|)D\�)D]|)D]�)D^|)D^�)D_|)D_�)D`|)D`�)Da|)Da�)Db|)Db�)Dc|)Dc�)Dd|)Dd�)De|)De�)Df|)Df�)Dg|)Dg�)Dh|)Dh�)Di|)Di�)Dj|)Dj�)Dk|)Dk�)Dl|)Dl�)Dm|)Dm�)Dn|)Dn�)Do|)Do�)Dp|)Dp�)Dq|)Dq�)Dr|)Dr�)Ds|)Ds�)Dt|)Dt��Dy��D��D�QGD���D��D�zD�>D��zD��zD�GD�AGD��zDǴzD��D�G�DڄzD�׮D��D�G�D�~D��z111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�ĜA���AڶFAڣ�Aڟ�AړuAډ7Aڇ+AڅAڅAڅAڃA�~�A�~�A�z�A�v�A�v�A�v�A�v�A�r�A�p�A�n�A�hsA�dZA�XA�G�A���A��/A�VA��A�n�Aɲ-A��AžwAã�A��TA�?}A�9XA��PA���A�;dA�(�A��A���A��A���A�E�A���A�M�A��A�Q�A� �A�~�A�M�A���A�+A�p�A�K�A� �A��A��9A�^5A��A���A�33A�33A��A��mA���A�K�A��^A�7LA��RA���A�1'A�bNA�K�A�1'A�ĜA��A���A���A��hA�$�A�r�A��-A��uA�v�A�hsA��\A��yA���A�t�A��A�VA�C�A�r�A|v�Ax��AuC�Asl�Ap��Ao�;An�Am�#Al�9AjbAh$�Af�/Af{Ad�jAb�\A`�A^��A^ZA]��A\�`A[?}AZ�AY��AU&�AQ�AP��AN�yAL��ALbNAK|�AJ-AI�AIp�AH�+AH-AG��AFn�AE�wAE��AEt�AE/AD��AC�wABffAA�^AAx�AAK�A@n�A??}A=��A<��A<1A:��A9A7?}A6{A4r�A4=qA2r�A0bA.��A-?}A++A)�7A'�A&-A$��A#XA!l�A  �A�A��AO�A/A|�A^5AG�AȴA{AO�A��AI�AK�A�A;dA�AJA
��A	�-A�A\)A��A�/AjAbA�FA&�A�+A��AhsA �!A {@�{@� �@�
=@�5?@�  @�\)@�"�@�ȴ@�E�@�r�@�C�@�+@��@��@��@�=q@홚@�@��@��@�ȴ@�h@��;@�"�@旍@�V@�%@��;@���@�`B@�Z@�|�@�o@ް!@���@�/@��m@�
=@�E�@�G�@��@أ�@�bN@�\)@֧�@�E�@�J@պ^@Ձ@��@�1@��#@�Ĝ@�b@Ϯ@�S�@�
=@Χ�@��#@̼j@�  @��@�=q@��#@Ɂ@�7L@���@��@Ǖ�@�33@ƸR@�J@őh@��@�Z@��m@���@þw@�@���@���@�j@�S�@�V@���@��@��@���@��w@�"�@�^5@��@��^@�p�@�9X@��;@�5?@��#@�`B@�?}@���@���@�1'@��@��F@���@�1@�;d@�"�@�+@��@��+@�`B@��D@��@��@�l�@�dZ@�|�@�S�@��@���@�^5@��@�O�@��@�1@�b@��@�+@���@���@��-@�x�@�G�@���@�z�@��@���@�@�n�@��@�@���@��7@�G�@�%@��`@�Ĝ@���@�I�@�b@��m@��
@���@�t�@�\)@�+@�o@��@��R@��\@�ff@�@�@��7@�/@��/@��j@���@�z�@�9X@��;@�|�@�33@���@�ȴ@��R@���@�~�@��@��#@���@�@���@�x�@�G�@��@���@�I�@���@�dZ@�o@�@�@���@��\@�~�@�~�@�M�@�@���@��h@�x�@�&�@���@��`@��j@��@���@��D@�Z@�Q�@�A�@��@�  @��
@��F@��P@��P@�|�@�t�@�dZ@�C�@�o@��y@��R@�^5@�J@��@���@���@�hs@�V@��j@��D@�bN@�1@���@��@��@�dZ@�"�@��y@��y@���@��\@�{@�@��#@��-@�x�@�p�@�p�@�hs@�X@��@��j@���@��@�r�@�Z@� �@�1@���@��;@���@�dZ@�+@�33@��@�n�@�M�@�5?@��@��^@�O�@��@���@���@���@�z�@�Z@�A�@� �@���@���@�t�@�@��\@���@�1'@�  @v$�@l��@c@]��@S33@M��@G�@@�@<(�@7+@/��@)�7@#�m@ b@n�@��@�R@	�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�ĜA���AڶFAڣ�Aڟ�AړuAډ7Aڇ+AڅAڅAڅAڃA�~�A�~�A�z�A�v�A�v�A�v�A�v�A�r�A�p�A�n�A�hsA�dZA�XA�G�A���A��/A�VA��A�n�Aɲ-A��AžwAã�A��TA�?}A�9XA��PA���A�;dA�(�A��A���A��A���A�E�A���A�M�A��A�Q�A� �A�~�A�M�A���A�+A�p�A�K�A� �A��A��9A�^5A��A���A�33A�33A��A��mA���A�K�A��^A�7LA��RA���A�1'A�bNA�K�A�1'A�ĜA��A���A���A��hA�$�A�r�A��-A��uA�v�A�hsA��\A��yA���A�t�A��A�VA�C�A�r�A|v�Ax��AuC�Asl�Ap��Ao�;An�Am�#Al�9AjbAh$�Af�/Af{Ad�jAb�\A`�A^��A^ZA]��A\�`A[?}AZ�AY��AU&�AQ�AP��AN�yAL��ALbNAK|�AJ-AI�AIp�AH�+AH-AG��AFn�AE�wAE��AEt�AE/AD��AC�wABffAA�^AAx�AAK�A@n�A??}A=��A<��A<1A:��A9A7?}A6{A4r�A4=qA2r�A0bA.��A-?}A++A)�7A'�A&-A$��A#XA!l�A  �A�A��AO�A/A|�A^5AG�AȴA{AO�A��AI�AK�A�A;dA�AJA
��A	�-A�A\)A��A�/AjAbA�FA&�A�+A��AhsA �!A {@�{@� �@�
=@�5?@�  @�\)@�"�@�ȴ@�E�@�r�@�C�@�+@��@��@��@�=q@홚@�@��@��@�ȴ@�h@��;@�"�@旍@�V@�%@��;@���@�`B@�Z@�|�@�o@ް!@���@�/@��m@�
=@�E�@�G�@��@أ�@�bN@�\)@֧�@�E�@�J@պ^@Ձ@��@�1@��#@�Ĝ@�b@Ϯ@�S�@�
=@Χ�@��#@̼j@�  @��@�=q@��#@Ɂ@�7L@���@��@Ǖ�@�33@ƸR@�J@őh@��@�Z@��m@���@þw@�@���@���@�j@�S�@�V@���@��@��@���@��w@�"�@�^5@��@��^@�p�@�9X@��;@�5?@��#@�`B@�?}@���@���@�1'@��@��F@���@�1@�;d@�"�@�+@��@��+@�`B@��D@��@��@�l�@�dZ@�|�@�S�@��@���@�^5@��@�O�@��@�1@�b@��@�+@���@���@��-@�x�@�G�@���@�z�@��@���@�@�n�@��@�@���@��7@�G�@�%@��`@�Ĝ@���@�I�@�b@��m@��
@���@�t�@�\)@�+@�o@��@��R@��\@�ff@�@�@��7@�/@��/@��j@���@�z�@�9X@��;@�|�@�33@���@�ȴ@��R@���@�~�@��@��#@���@�@���@�x�@�G�@��@���@�I�@���@�dZ@�o@�@�@���@��\@�~�@�~�@�M�@�@���@��h@�x�@�&�@���@��`@��j@��@���@��D@�Z@�Q�@�A�@��@�  @��
@��F@��P@��P@�|�@�t�@�dZ@�C�@�o@��y@��R@�^5@�J@��@���@���@�hs@�V@��j@��D@�bN@�1@���@��@��@�dZ@�"�@��y@��y@���@��\@�{@�@��#@��-@�x�@�p�@�p�@�hs@�X@��@��j@���@��@�r�@�Z@� �@�1@���@��;@���@�dZ@�+@�33@��@�n�@�M�@�5?@��@��^@�O�@��@���@���@���@�z�@�Z@�A�@� �@���@���@�t�@�@��\@���@�1'@�  @v$�@l��@c@]��@S33@M��@G�@@�@<(�@7+@/��@)�7@#�m@ b@n�@��@�R@	�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�\B�\B�\B�bB�bB�bB�hB�hB�oB�oB�oB�oB�oB�uB�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B�B��B�qB�wB�XB�dB�FB�B�B��B��B��B��B�7By�Bn�B\)BI�B49B%�B�B�B	7B��B�B�ZB�#B��B�}B�B�B��B��B��B��B��B�VB�B|�BjBN�BD�B;dB33B+B�BoBB�B��B�RB�B��B�hBp�BffBXBG�B2-B�BB
�B
�ZB
��B
�3B
��B
�JB
s�B
iyB
K�B
-B
hB
B	��B	�B	�fB	�;B	�B	��B	��B	�XB	�3B	��B	��B	��B	�JB	�7B	�%B	~�B	v�B	r�B	iyB	W
B	H�B	D�B	=qB	5?B	2-B	.B	'�B	&�B	#�B	 �B	�B	�B	�B	�B	�B	{B	uB	hB	PB		7B	+B	%B	B	B��B��B�B�B�mB�;B�)B�B��B��B��BÖB��B�dB�9B�B��B��B��B��B��B��B�oB�bB�PB�7B�B�B�B�B~�B}�B{�By�Bx�Bv�Bt�Bs�Br�Bo�Bo�Bm�Bk�BjBjBjBiyBiyBiyBiyBjBjBk�Bk�Bk�BhsBhsBm�Bl�Bl�Bn�Bn�Bn�Bp�Bo�Bn�Bp�Br�Bp�Bo�Bu�B{�B}�B|�B|�Bz�B{�B~�B~�B~�B}�B}�By�Bw�Bz�B~�B�B�B�B�B�B�%B�7B�uB�{B�{B�{B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�!B�-B�9B�FB�RB�dB�qB�}BBŢBǮBɺB��B��B��B��B�B�B�B�B�#B�;B�TB�`B�`B�`B�mB�B�B�B��B��B��B	B	B	%B	hB	{B	{B	�B	�B	�B	�B	#�B	&�B	&�B	'�B	)�B	5?B	49B	6FB	9XB	=qB	?}B	A�B	B�B	C�B	F�B	J�B	O�B	T�B	[#B	_;B	dZB	e`B	cTB	ffB	jB	l�B	p�B	q�B	r�B	t�B	u�B	s�B	u�B	y�B	z�B	{�B	|�B	}�B	|�B	|�B	}�B	~�B	~�B	� B	� B	�B	�B	�B	�B	�+B	�1B	�7B	�=B	�DB	�JB	�PB	�VB	�\B	�bB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�!B	�'B	�-B	�9B	�?B	�9B	�?B	�?B	�FB	�LB	�RB	�^B	�jB	�}B	��B	��B	��B	��B	ÖB	ÖB	ĜB	ŢB	ƨB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�)B	�/B	�/B	�5B	�;B	�HB	�NB	�TB	�ZB	�`B	�fB	�fB	�mB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
VB
�B
�B
%�B
+B
33B
7LB
>wB
B�B
E�B
K�B
O�B
T�B
YB
^5B
bNB
ffB
k�B
o�B
t�B
y�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�dB�eB�cB�jB�lB�nB�rB�rB�xB�xB�vB�vB�xB�~B�zB��B��B��B��B��B��B��B��B��B��B��B��B�B�B��B�}B�B�dB�oB�OB�B�B��B��B��B��B�>By�Bn�B\2BI�B4CB%�B�B�B	>B��B�B�bB�+B��B��B� B�B��B��B��B��B��B�_B�B|�Bj�BN�BD�B;kB39B+B�BuB%B�B��B�VB�B��B�mBp�BfkBXBG�B24B�BB
�B
�bB
��B
�<B
��B
�UB
s�B
i�B
K�B
-B
vB
B	��B	��B	�wB	�JB	� B	��B	��B	�jB	�DB	�B	��B	��B	�[B	�IB	�8B	B	v�B	r�B	i�B	WB	H�B	D�B	=�B	5UB	2BB	.,B	(B	&�B	#�B	 �B	�B	�B	�B	�B	�B	�B	�B	B	hB		PB	BB	:B	0B	B�B��B��B��B�B�VB�AB�/B�B��B��BïB��B�B�UB�0B�B��B��B��B��B��B��B��B�nB�WB�;B�/B�&B�$BB~B|By�Bx�Bv�Bt�Bs�Br�Bo�Bo�Bm�Bk�Bj�Bj�Bj�Bi�Bi�Bi�Bi�Bj�Bj�Bk�Bk�Bk�Bh�Bh�Bm�Bl�Bl�Bn�Bn�Bn�Bp�Bo�Bn�Bp�Br�Bp�Bo�Bu�B|B~B}	B}Bz�B|BBBB~B~By�Bw�Bz�BB�!B�#B�/B�-B�3B�BB�TB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�$B�(B�/B�<B�GB�TB�aB�jB�~B��B��BªBźB��B��B��B��B��B�	B�B�(B�/B�2B�<B�RB�mB�vB�xB�yB�B�B�B��B��B��B�B	B	*B	;B	�B	�B	�B	�B	�B	�B	�B	#�B	&�B	' B	(B	*B	5TB	4NB	6ZB	9kB	=�B	?�B	A�B	B�B	C�B	F�B	J�B	O�B	UB	[6B	_MB	dnB	erB	cgB	fwB	j�B	l�B	p�B	q�B	r�B	t�B	u�B	s�B	u�B	y�B	z�B	{�B	}B	~B	}B	}B	~B	B	B	�B	�B	�B	�B	�&B	�*B	�=B	�BB	�GB	�MB	�UB	�\B	�`B	�gB	�nB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�+B	�4B	�3B	�7B	�<B	�IB	�NB	�IB	�NB	�OB	�VB	�]B	�eB	�oB	�{B	��B	��B	��B	��B	��B	äB	æB	īB	űB	ƷB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�+B	�9B	�=B	�?B	�EB	�JB	�XB	�\B	�cB	�hB	�oB	�xB	�uB	�zB	�|B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�B	�B	�	B
 B
B
B
B
B
B
B
B
B
B
B
B
#B
 B
*B
*B
cB
�B
�B
%�B
+B
3AB
7YB
>�B
B�B
E�B
K�B
O�B
UB
Y"B
^AB
b\B
fpB
k�B
o�B
t�B
y�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.06 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214402016053112144020160531121440  AO  ARCAADJP                                                                    20141120130048    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20141120130048  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20141120130048  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121440  IP                  G�O�G�O�G�O�                