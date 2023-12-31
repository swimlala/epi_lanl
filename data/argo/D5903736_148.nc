CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-05-16T09:15:19Z AOML 3.0 creation; 2016-05-31T19:14:49Z UW 3.1 conversion     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         (Matthew Alkire, University of Washington      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7$   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7(   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7,   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7<   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7L   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7\   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7d   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8    DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8@   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8D   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8H   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8h   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
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
_FillValue                    �    HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �P   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �`   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �d   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �t   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �x   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �|   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20160516091519  20190604094001  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051_7090_148                   2C  D   APEX                            5368                            041511                          846 @׬���j�1   @׬�9�H@2�t�j~��d3n��P1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BO��BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  Dy�D  D� D��D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDy� D�  D�<�D�i�D�Y�D�3D�FfD�vfD���D�3D�6fD���D��3D���D�C3Dڌ�D�� D� D�0 D�y�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�z�@�{@�{A
=A?
=A_
=A
=A��A��A��A��AυA߅A�A��BBBBB'B/B7B?BGBO\)BWB_BgBoBwBB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB�{B��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HC�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RD |)D �)D|)D�)D|)D�)Du�D�)D|)D��D|)D�)D|)D�)D|)D�)D|)D�)D	|)D	�)D
|)D
�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D |)D �)D!|)D!�)D"|)D"�)D#|)D#�)D$|)D$�)D%|)D%�)D&|)D&�)D'|)D'�)D(|)D(�)D)|)D)�)D*|)D*�)D+|)D+�)D,|)D,�)D-|)D-�)D.|)D.�)D/|)D/�)D0|)D0�)D1|)D1�)D2|)D2�)D3|)D3�)D4|)D4�)D5|)D5�)D6|)D6�)D7|)D7�)D8|)D8�)D9|)D9�)D:|)D:�)D;|)D;�)D<|)D<�)D=|)D=�)D>|)D>�)D?|)D?�)D@|)D@�)DA|)DA�)DB|)DB�)DC|)DC�)DD|)DD�)DE|)DE�)DF|)DF�)DG|)DG�)DH|)DH�)DI|)DI�)DJ|)DJ�)DK|)DK�)DL|)DL�)DM|)DM�)DN|)DN�)DO|)DO�)DP|)DP�)DQ|)DQ�)DR|)DR�)DS|)DS�)DT|)DT�)DU|)DU�)DV|)DV�)DW|)DW�)DX|)DX�)DY|)DY�)DZ|)DZ�)D[|)D[�)D\|)D\�)D]|)D]�)D^|)D^�)D_|)D_�)D`|)D`�)Da|)Da�)Db|)Db�)Dc|)Dc�)Dd|)Dd�)De|)De�)Df|)Df�)Dg|)Dg�)Dh|)Dh�)Di|)Di�)Dj|)Dj�)Dk|)Dk�)Dl|)Dl�)Dm|)Dm�)Dn|)Dn�)Do|)Do�)Dp|)Dp�)Dq|)Dq�)Dr|)Dr�)Ds|)Ds�)Dt|)Dt�Dy�)D��D�:�D�g�D�W�D�GD�DzD�tzD���D�GD�4zD���D��GD���D�AGDڊ�D��D�D�.D�w�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�G�A�1'A��mA�|�A�^5A�G�A�&�A��A�1A��;AѶFAѝ�A�\)A��mA�ȴA���Aк^A�v�A��Aϥ�A��;A�A�=qA�n�A��A���Aȴ9A�Q�A��HA�x�AƗ�A�$�A�G�Ağ�A��A�VA�VA�&�A�+A���A��A��A�
=A�G�A�{A��#A�l�A�(�A�l�A��
A���A�7LA�33A���A��TA�ZA�1A��RA�C�A�Q�A�  A�  A�Q�A�E�A��A���A�;dA�  A�n�A��-A���A�ƨA�z�A�VA��FA�A���A�?}A��!A�oA�I�A��mA�JA�XA���A��-A��A�x�A�~�A�p�A���A�ZA��7A��A�`BA���A���A��RA�A���A�ȴA�/A��A{&�At�Ap��Al�RAj�Ah��Agl�AfĜAe��AeS�Adz�AaK�A_��A^��A^��A^$�A\M�AX�HAV�AT��AT�9ATr�ASVAPM�AO�PAN�\AL1'AJ�AJ-AI��AHJAG+AFA�AB�9AB~�AAp�A>-A<�A;
=A:I�A9A97LA7hsA61A4�DA2�jA1A0��A0�A.��A)\)A(��A(��A(1A%�mA$��A#�7A"�A!�A E�A&�A+AA�A�hAv�A�`AG�A��A�AO�Ap�A=qA�PAn�A��A�hA�A�AoA1'AG�A
�jA
I�A
1AbNA�PAK�A�A(�AhsAbA&�A�9A�7A �A v�@��
@�"�@�V@�hs@�9X@�t�@��-@���@�l�@��@��@��m@�$�@�Ĝ@��y@���@�u@���@�@畁@��y@��#@�&�@�j@�C�@�h@�t�@݉7@�?}@ܣ�@��
@�K�@�M�@ٙ�@��m@֗�@�X@Դ9@�1@ӝ�@�@�r�@�@�@��@̣�@̛�@���@̃@�dZ@�n�@�%@�j@���@�+@��H@�5?@�V@ă@Õ�@�v�@�{@�7L@� �@��y@�ff@�v�@�n�@���@��;@�l�@�@���@�n�@��@�p�@��/@���@���@��R@�~�@�{@���@�O�@��@���@��
@�t�@�dZ@�K�@�n�@�p�@���@�1'@��;@�\)@���@�5?@�-@��@�G�@��u@�ƨ@��P@�S�@�@��\@�J@��@�X@�Z@��w@��@���@�n�@��@��#@��@�ff@�$�@�X@��@�9X@��!@�E�@�V@�n�@�v�@�V@�~�@��\@�^5@�X@��F@��H@�K�@�ȴ@�33@��@�/@��`@�Z@�;d@�"�@���@���@�~�@��7@�X@�x�@���@���@��-@��D@�j@��@��w@��9@���@�A�@��@�"�@�p�@���@�A�@�1@��@�l�@�;d@�o@�
=@�"�@�o@���@�~�@�ff@�~�@���@���@��+@�J@��@���@��@��D@�Q�@� �@� �@���@�K�@�"�@�ȴ@�V@�^5@��\@��\@�C�@���@���@��@��F@�ƨ@���@��
@��P@�
=@���@���@�G�@�&�@�V@�V@��@�G�@���@���@��#@��^@�p�@��@�j@�(�@�|�@�t�@��@�t�@��y@���@�ȴ@��R@�M�@�@�?}@���@��@�/@���@���@���@�5?@���@���@��-@���@��h@�`B@�V@���@��D@�bN@�bN@�Z@�j@��j@���@��j@���@���@��j@���@��D@�Z@�I�@���@�33@�ȴ@�@��@�`B@�&�@���@��@�Ĝ@��@�r�@�I�@�1@��;@��
@��w@�dZ@�\)@�K�@��@���@�n�@��@�J@�^5@�\)@�Q�@y��@p��@g�P@^v�@U/@N��@J-@B-@:��@2��@+S�@%�@  �@33@�@I�@%@��@	�^@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�G�A�1'A��mA�|�A�^5A�G�A�&�A��A�1A��;AѶFAѝ�A�\)A��mA�ȴA���Aк^A�v�A��Aϥ�A��;A�A�=qA�n�A��A���Aȴ9A�Q�A��HA�x�AƗ�A�$�A�G�Ağ�A��A�VA�VA�&�A�+A���A��A��A�
=A�G�A�{A��#A�l�A�(�A�l�A��
A���A�7LA�33A���A��TA�ZA�1A��RA�C�A�Q�A�  A�  A�Q�A�E�A��A���A�;dA�  A�n�A��-A���A�ƨA�z�A�VA��FA�A���A�?}A��!A�oA�I�A��mA�JA�XA���A��-A��A�x�A�~�A�p�A���A�ZA��7A��A�`BA���A���A��RA�A���A�ȴA�/A��A{&�At�Ap��Al�RAj�Ah��Agl�AfĜAe��AeS�Adz�AaK�A_��A^��A^��A^$�A\M�AX�HAV�AT��AT�9ATr�ASVAPM�AO�PAN�\AL1'AJ�AJ-AI��AHJAG+AFA�AB�9AB~�AAp�A>-A<�A;
=A:I�A9A97LA7hsA61A4�DA2�jA1A0��A0�A.��A)\)A(��A(��A(1A%�mA$��A#�7A"�A!�A E�A&�A+AA�A�hAv�A�`AG�A��A�AO�Ap�A=qA�PAn�A��A�hA�A�AoA1'AG�A
�jA
I�A
1AbNA�PAK�A�A(�AhsAbA&�A�9A�7A �A v�@��
@�"�@�V@�hs@�9X@�t�@��-@���@�l�@��@��@��m@�$�@�Ĝ@��y@���@�u@���@�@畁@��y@��#@�&�@�j@�C�@�h@�t�@݉7@�?}@ܣ�@��
@�K�@�M�@ٙ�@��m@֗�@�X@Դ9@�1@ӝ�@�@�r�@�@�@��@̣�@̛�@���@̃@�dZ@�n�@�%@�j@���@�+@��H@�5?@�V@ă@Õ�@�v�@�{@�7L@� �@��y@�ff@�v�@�n�@���@��;@�l�@�@���@�n�@��@�p�@��/@���@���@��R@�~�@�{@���@�O�@��@���@��
@�t�@�dZ@�K�@�n�@�p�@���@�1'@��;@�\)@���@�5?@�-@��@�G�@��u@�ƨ@��P@�S�@�@��\@�J@��@�X@�Z@��w@��@���@�n�@��@��#@��@�ff@�$�@�X@��@�9X@��!@�E�@�V@�n�@�v�@�V@�~�@��\@�^5@�X@��F@��H@�K�@�ȴ@�33@��@�/@��`@�Z@�;d@�"�@���@���@�~�@��7@�X@�x�@���@���@��-@��D@�j@��@��w@��9@���@�A�@��@�"�@�p�@���@�A�@�1@��@�l�@�;d@�o@�
=@�"�@�o@���@�~�@�ff@�~�@���@���@��+@�J@��@���@��@��D@�Q�@� �@� �@���@�K�@�"�@�ȴ@�V@�^5@��\@��\@�C�@���@���@��@��F@�ƨ@���@��
@��P@�
=@���@���@�G�@�&�@�V@�V@��@�G�@���@���@��#@��^@�p�@��@�j@�(�@�|�@�t�@��@�t�@��y@���@�ȴ@��R@�M�@�@�?}@���@��@�/@���@���@���@�5?@���@���@��-@���@��h@�`B@�V@���@��D@�bN@�bN@�Z@�j@��j@���@��j@���@���@��j@���@��D@�Z@�I�@���@�33@�ȴ@�@��@�`B@�&�@���@��@�Ĝ@��@�r�@�I�@�1@��;@��
@��w@�dZ@�\)@�K�@��@���@�n�@��@�J@�^5G�O�@�Q�@y��@p��@g�P@^v�@U/@N��@J-@B-@:��@2��@+S�@%�@  �@33@�@I�@%@��@	�^@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
��B
��B
�B
�B
��B
��B
��B
��BBPB�B�B/BL�BS�B^5BgmBy�By�Bz�B{�B�%B��B�XBB�fBDB�B�B$�B1'B9XBYBr�B�bB��B�jB��B�}BƨB�B�B��B�B$�B&�B)�B'�B%�B(�B)�B(�B0!B5?B6FB6FB49B2-B,B#�B�B'�B%�B"�B�BPB�fB�3Bx�BM�BiyB�JB�VB�%B}�Bv�BdZBN�B:^B.B�B�B�HB�BƨB�LB�B��Bx�BjB_;BN�B>wB,BuB
�yB
ɺB
�LB
��B
��B
�=B
x�B
bNB
@�B
{B	�B	��B	ĜB	ɺB	��B	B	�XB	�-B	�B	��B	��B	��B	��B	�hB	�+B	t�B	gmB	\)B	ZB	W
B	M�B	>wB	9XB	2-B	%�B	�B	�B	�B	VB		7B	B�B�B�B�;B�B��B��B��B��BƨB��B�qB�RB�?B�-B�B��B��B��B��B��B�hB�\B�PB�=B�+B�B�B}�B{�By�Bw�Bu�Bt�Bs�Bp�Bn�Bm�Bm�Bm�Bn�Bn�Bo�Bo�Bo�Bp�Bo�Bn�Bn�Bp�Br�Bp�Bq�Bq�Bq�B|�B|�By�Bw�Bu�Bu�Bt�Bt�Bw�Bw�Bx�Bx�By�Bz�B|�B{�Bz�Bz�B{�By�Bx�Bz�B|�B}�B� B�%B�7B�1B�+B�VB�oB�oB�uB��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B��B��B��B��B��B��B��B��B�B�B�B�B�'B�3B�?B�?B�?B�LB�XB�qBǮBǮBǮBɺB��B��B�
B�)B�B�B�#B�5B�NB�ZB�`B�mB�B�B�B�B�B��B��B��B��B��B	B	B	B	B		7B	bB	uB	�B	�B	�B	 �B	"�B	#�B	&�B	.B	2-B	6FB	6FB	8RB	;dB	B�B	G�B	K�B	K�B	N�B	O�B	P�B	S�B	R�B	R�B	R�B	VB	[#B	\)B	bNB	dZB	dZB	ffB	gmB	hsB	jB	l�B	o�B	q�B	q�B	s�B	r�B	l�B	k�B	n�B	n�B	t�B	}�B	�B	�%B	�7B	�DB	�JB	�PB	�VB	�VB	�JB	�DB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�-B	�-B	�9B	�LB	�jB	�}B	B	ĜB	ƨB	ǮB	��B	��B	��B	��B	�B	�B	�#B	�)B	�/B	�5B	�;B	�;B	�5B	�/B	�/B	�B	�B	�B	�B	�B	�B	�#B	�/B	�;B	�BB	�HB	�HB	�HB	�BB	�HB	�BB	�BB	�TB	�mB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
1B
\B
DB
�B
$�B
,B
1'B
9XB
@�B
F�B
N�B
T�B
]/B
cTB
hsB
l�B
p�B
t�B
w�B
z�B
~�B
�B
�%111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
�zB
�xB
�nB
�nB
�sB
�}B
��B
��B�BBHBqB.�BL�BS�B]�Bg%By�By�Bz�B{�B��B��B�B�IB�B B8BoB$�B0�B9BX�BriB�B��B�!B�BB�7B�`B��B�_B��B[B$�B&�B)�B'�B%�B(�B)�B(�B/�B4�B6B5�B3�B1�B+�B#�BuB'�B%�B"�B`BB�B��Bx�BM�Bi1B�B�B��B}�Bv~BdBN�B:B-�BbB�jB�BվB�fB�B��B�FBx�Bj8B^�BN�B>/B+�B.B
�6B
�rB
�B
��B
�ZB
��B
x�B
bB
@?B
6B	�_B	͎B	�VB	�uB	�>B	�IB	�B	��B	��B	��B	�bB	�QB	�CB	�!B	��B	t|B	g)B	[�B	Y�B	V�B	M�B	>2B	9B	1�B	%�B	uB	_B	EB	B	�B	�B�sB�^B�7B��B��BӷBѫBϞB˂B�eB�DB�/B�B��B��B��B��B�fB�ZB�OB�?B�&B�B�B��B��B��B��B}�B{�By�Bw�Bu�BtyBswBp`BnXBmPBmOBmPBnWBnSBoYBo^Bo\BpdBoZBnRBnWBpcBrmBpbBqiBqlBqgB|�B|�By�Bw�Bu�Bu�Bt�Bt{Bw�Bw�Bx�Bx�By�Bz�B|�B{�Bz�Bz�B{�By�Bx�Bz�B|�B}�B�B��B��B��B��B�B�-B�+B�0B�BB�WB�uB�uB�uB�~B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�1B�mB�mB�oB�xBΖBҳB��B��B��B��B��B��B�B�B�B�+B�;B�OB�ZB�cB�lB��B��B��B��B��B	 �B	�B	�B	�B	�B	B	3B	JB	gB	rB	 B	"�B	#�B	&�B	-�B	1�B	6B	6B	8B	; B	BLB	GlB	K�B	K�B	N�B	O�B	P�B	S�B	R�B	R�B	R�B	U�B	Z�B	[�B	b
B	dB	dB	f"B	g+B	h2B	j<B	lJB	o]B	qfB	qlB	stB	rmB	lIB	kBB	nWB	nWB	tzB	}�B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�(B	�QB	�NB	�MB	�GB	�dB	�XB	�hB	��B	��B	��B	��B	��B	�qB	�\B	�`B	�fB	�lB	�lB	�qB	�wB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�&B	�<B	�LB	�XB	�iB	�mB	͔B	ѪB	ҮB	ӳB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	�B	�B	� B	�B	�&B	�*B	�8B	�<B	�BB	�LB	�LB	�CB	�=B	�>B	�WB	�^B	�EB	�CB	�CB	�JB	�RB	�OB	�OB	�LB	�ZB	�bB	�dB	�hB	�nB	�vB	�wB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
�B
 �B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
�B
�B
�G�O�B
B
hB
$�B
+�B
0�B
9B
@?B
FfB
N�B
T�B
\�B
cB
h3B
lIB
pbB
tyB
w�B
z�B
~�B
��B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.06 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =1(+/-0.0001), vertically averaged dS =0(+/-0.002) in PSS-78.                                                                                                                                                                                                 Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Map scales: x=6,3; y=2,1. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                            201906040940012019060409400120190604094001  AO  ARCAADJP                                                                    20160516091519    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160516091519  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160516091519  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604094001  IP                  G�O�G�O�G�O�                