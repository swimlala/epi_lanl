CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-01-14T10:15:56Z AOML 3.0 creation; 2016-05-31T19:14:47Z UW 3.1 conversion     
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
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �,   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �<   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �@   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �P   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �T   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �X   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �\Argo profile    3.1 1.2 19500101000000  20160114101556  20190604093959  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051_7090_136                   2C  D   APEX                            5368                            041511                          846 @׍�zO�)1   @׍��>@4��j~���df�G�{1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  C   C�C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DOfDO� DPfDP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dtl�Dyl�D��3D�L�D���D��3D���D�@ D��3D���D� D�C3D���D�� D�	�D�0 D�s3D�ٚD�fD�FfD�y�D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@�{@�{A
=A?
=A_
=A
=A��A��A��A��AυA߅A�A��BBBBB'B/B7B?BGBOBWB_BgBoBwBB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��B��HB��HB��HB��HB��HB��HB��HB��HBۮB��HB��HB��HB��HB��HB��HB��HB��HB��HC
>C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RD |)D �)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D	|)D	�)D
|)D
�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D |)D �)D!|)D!�)D"|)D"�)D#|)D#�)D$|)D$�)D%|)D%�)D&|)D&�)D'|)D'�)D(|)D(�)D)|)D)�)D*|)D*�)D+|)D+�)D,|)D,�)D-|)D-�)D.|)D.�)D/|)D/�)D0|)D0�)D1|)D1�)D2|)D2�)D3|)D3�)D4|)D4�)D5|)D5�)D6|)D6�)D7|)D7�)D8|)D8�)D9|)D9�)D:|)D:�)D;|)D;�)D<|)D<�)D=|)D=�)D>|)D>�)D?|)D?�)D@|)D@�)DA|)DA�)DB|)DB�)DC|)DC�)DD|)DD�)DE|)DE�)DF|)DF�)DG|)DG�)DH|)DH�)DI|)DI�)DJ|)DJ�)DK|)DK�)DL|)DL�)DM|)DM�)DN|)DO�DO|)DP�DP|)DP�)DQ|)DQ�)DR|)DR�)DS|)DS�)DT|)DT�)DU|)DU�)DV|)DV�)DW|)DW�)DX|)DX�)DY|)DY�)DZ|)DZ�)D[|)D[�)D\|)D\�)D]|)D]�)D^|)D^�)D_|)D_�)D`|)D`�)Da|)Da�)Db|)Db�)Dc|)Dc�)Dd|)Dd�)De|)De�)Df|)Df�)Dg|)Dg�)Dh|)Dh�)Di|)Di�)Dj|)Dj�)Dk|)Dk�)Dl|)Dl�)Dm|)Dm�)Dn|)Dn�)Do|)Do�)Dp|)Dp�)Dq|)Dq�)Dr|)Dr�)Ds|)Ds�)Dth�Dyh�D��GD�J�D���D��GD���D�>D��GD���D�D�AGD���DǾD��D�.D�qGD�׮D�zD�DzD�w�D��z11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A���A�t�A�E�A�9XA�/A�JA���A�\)A�VA���A�dZA���A��A�A�9XA�A�AƾwA��AŁA�7LAľwA�ffA�;dA��yA�x�A�A\A�hsA��yA��7A��A�x�A�n�A�l�A�bNA�S�A��A��mA�G�A���A�{A��7A�(�A�dZA��A�ȴA��A���A�r�A���A�v�A���A�VA�9XA���A�=qA��A�\)A�C�A��A���A�hsA�G�A�1'A�5?A�dZA�E�A���A���A�M�A���A�"�A��A��A���A���A�$�A���A��
A�JA�
=A���A���A��;A�;dA��9A�bNA��!A�ĜA��hA���A�$�A�M�A��jA�S�A���A��;A���A��mA��#A��HA�1'A�ĜA�A�A��A��TA���A��A��/A�bNA�l�A
=A}/Az�DAx�/At��ArbAm�#AkK�Ah�uAf��Ae+Ac"�A`A�A]�AZ  AW?}AS��AR-APz�AN1'ALI�AJQ�AH9XAFVAEK�ADZAC��AB��AA�A?;dA<�A:�RA933A7�A7
=A6bA5�A4A�A3hsA2��A2 �A1�-A0�`A.�DA-A,��A+�-A* �A)7LA(�uA(ffA(1'A&�A$��A#�A#�A"M�A"A!t�A ��A �jA ffA $�A�;A��A/A��A�#A
=A-A��AVAĜA�\AA�A�hA��A�#A$�A�A�AbNA��A7LA��Ar�A��A33A%A-A`BA�mAx�A
�A
n�A	/A	G�AA�A/A5?A�TAoA1A�TA��A�A�7A A�@�t�@�v�@�A�@���@���@�@�J@�  @�$�@��@���@�w@��@�V@��T@�D@旍@��@�ƨ@�V@�1'@�\)@�ff@�X@�`B@�G�@��@ف@�&�@��@�
=@�E�@���@���@�p�@���@� �@Ӿw@�33@ҧ�@�X@���@�O�@��@̴9@��y@ɉ7@��@Ǿw@�\)@��y@Ɵ�@�7L@þw@��@��#@��9@�9X@� �@�  @��@�1'@�I�@�ƨ@�A�@�r�@�(�@��P@�
=@��!@���@�ff@�M�@���@���@��@�A�@�1@��@�ƨ@�\)@���@�V@�M�@�@�G�@��9@�1@��F@�t�@��@�-@���@�&�@��j@�(�@��;@���@��R@�$�@���@���@�`B@�O�@�&�@�Ĝ@�r�@�1'@���@���@�v�@���@��@��9@�bN@�1@�;d@�o@�o@�o@�
=@�
=@�o@��@�
=@���@��!@�M�@�@�?}@�(�@���@�C�@��@�~�@�5?@�{@���@�@�@�@���@��7@�X@�/@�V@�%@��/@���@�Z@���@���@�o@��@���@�~�@�E�@���@�?}@��/@�z�@�b@���@��@�;d@��@��@���@�^5@��@�@��h@��7@�&�@���@��@���@�z�@�bN@�A�@�(�@���@�\)@�K�@�"�@�ȴ@�^5@���@��7@��h@��7@�/@�I�@�dZ@��@��@�G�@�V@���@�z�@�I�@��@�1@�  @�ƨ@��w@��
@��;@��m@��F@��@�l�@��@���@��
@�l�@�"�@�
=@�;d@�dZ@�S�@��H@�v�@�$�@�$�@��-@���@���@��@��P@�bN@�Q�@�l�@���@�ȴ@�t�@�1'@���@�o@���@�n�@��@��7@��@�Q�@�(�@�  @��@��R@�v�@��#@�p�@��7@�X@�/@�&�@�%@���@���@�(�@��@���@���@�t�@�@�^5@��@��h@�Ĝ@;d@r�H@ihs@cS�@]`B@VV@L��@E/@=�@5?}@0 �@+�F@%�@+@��@{@�#@?}@	�^@$�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A���A�t�A�E�A�9XA�/A�JA���A�\)A�VA���A�dZA���A��A�A�9XA�A�AƾwA��AŁA�7LAľwA�ffA�;dA��yA�x�A�A\A�hsA��yA��7A��A�x�A�n�A�l�A�bNA�S�A��A��mA�G�A���A�{A��7A�(�A�dZA��A�ȴA��A���A�r�A���A�v�A���A�VA�9XA���A�=qA��A�\)A�C�A��A���A�hsA�G�A�1'A�5?A�dZA�E�A���A���A�M�A���A�"�A��A��A���A���A�$�A���A��
A�JA�
=A���A���A��;A�;dA��9A�bNA��!A�ĜA��hA���A�$�A�M�A��jA�S�A���A��;A���A��mA��#A��HA�1'A�ĜA�A�A��A��TA���A��A��/A�bNA�l�A
=A}/Az�DAx�/At��ArbAm�#AkK�Ah�uAf��Ae+Ac"�A`A�A]�AZ  AW?}AS��AR-APz�AN1'ALI�AJQ�AH9XAFVAEK�ADZAC��AB��AA�A?;dA<�A:�RA933A7�A7
=A6bA5�A4A�A3hsA2��A2 �A1�-A0�`A.�DA-A,��A+�-A* �A)7LA(�uA(ffA(1'A&�A$��A#�A#�A"M�A"A!t�A ��A �jA ffA $�A�;A��A/A��A�#A
=A-A��AVAĜA�\AA�A�hA��A�#A$�A�A�AbNA��A7LA��Ar�A��A33A%A-A`BA�mAx�A
�A
n�A	/A	G�AA�A/A5?A�TAoA1A�TA��A�A�7A A�@�t�@�v�@�A�@���@���@�@�J@�  @�$�@��@���@�w@��@�V@��T@�D@旍@��@�ƨ@�V@�1'@�\)@�ff@�X@�`B@�G�@��@ف@�&�@��@�
=@�E�@���@���@�p�@���@� �@Ӿw@�33@ҧ�@�X@���@�O�@��@̴9@��y@ɉ7@��@Ǿw@�\)@��y@Ɵ�@�7L@þw@��@��#@��9@�9X@� �@�  @��@�1'@�I�@�ƨ@�A�@�r�@�(�@��P@�
=@��!@���@�ff@�M�@���@���@��@�A�@�1@��@�ƨ@�\)@���@�V@�M�@�@�G�@��9@�1@��F@�t�@��@�-@���@�&�@��j@�(�@��;@���@��R@�$�@���@���@�`B@�O�@�&�@�Ĝ@�r�@�1'@���@���@�v�@���@��@��9@�bN@�1@�;d@�o@�o@�o@�
=@�
=@�o@��@�
=@���@��!@�M�@�@�?}@�(�@���@�C�@��@�~�@�5?@�{@���@�@�@�@���@��7@�X@�/@�V@�%@��/@���@�Z@���@���@�o@��@���@�~�@�E�@���@�?}@��/@�z�@�b@���@��@�;d@��@��@���@�^5@��@�@��h@��7@�&�@���@��@���@�z�@�bN@�A�@�(�@���@�\)@�K�@�"�@�ȴ@�^5@���@��7@��h@��7@�/@�I�@�dZ@��@��@�G�@�V@���@�z�@�I�@��@�1@�  @�ƨ@��w@��
@��;@��m@��F@��@�l�@��@���@��
@�l�@�"�@�
=@�;d@�dZ@�S�@��H@�v�@�$�@�$�@��-@���@���@��@��P@�bN@�Q�@�l�@���@�ȴ@�t�@�1'@���@�o@���@�n�@��@��7@��@�Q�@�(�@�  @��@��R@�v�@��#@�p�@��7@�X@�/@�&�@�%@���@���@�(�@��@���@���@�t�@�@�^5@��@��h@�Ĝ@;d@r�H@ihs@cS�@]`B@VV@L��@E/@=�@5?}@0 �@+�F@%�@+@��@{@�#@?}@	�^@$�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B%B�B'�B<jBP�Bn�B�B��B��B�dB�5B�B�BBPB�B&�B0!B<jBI�BVBYB[#BcTBe`BgmBgmBgmBiyBiyBiyBgmBffBdZB]/BT�BN�BA�B1'B-B+B)�B$�B�B�BuBVB+B��B�)B��B��BȴBĜBɺB�BÖB��B��B�^B�?B�B��B��B��B�VB|�Bo�BffBL�BD�B<jB.B �BbBPB
=BB��B�B�sB�/B��BȴB�RB��B�Bz�Bt�Bn�Bk�Be`BYBJ�B<jB1'B�BB
�B
�5B
��B
ǮB
�XB
��B
�JB
{�B
m�B
^5B
O�B
0!B
uB	�B	��B	�wB	�9B	�B	��B	�B	t�B	_;B	N�B	?}B	7LB	0!B	$�B	�B	hB	1B	B��B��B��B�B�B�NB�B��B��BȴBŢBB�}B�jB�XB�LB�?B�3B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�uB�oB�oB�bB�hB�bB�\B�\B�VB�PB�JB�PB�DB�=B�\B�\B�bB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�\B�DB�1B�B� B|�B~�B�B�B�B�B� B�B�1B�DB�DB�DB�JB�JB�=B�JB�\B�hB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�9B�?B�9B�?B�LB�XB�dB�wB��BŢB��B��B��B�
B�B�B�;B�NB�NB�TB�TB�`B�sB�B�B�B�B�B�B�B�B��B��B��B	B	%B	+B		7B	JB	bB	uB	�B	�B	�B	 �B	!�B	&�B	+B	,B	-B	/B	0!B	1'B	33B	5?B	7LB	9XB	>wB	>wB	A�B	F�B	H�B	I�B	I�B	N�B	P�B	Q�B	Q�B	R�B	S�B	T�B	W
B	[#B	\)B	]/B	^5B	^5B	`BB	dZB	gmB	hsB	jB	l�B	o�B	r�B	v�B	w�B	w�B	x�B	y�B	{�B	}�B	~�B	�B	�B	�B	�B	�%B	�7B	�JB	�VB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�'B	�-B	�3B	�9B	�FB	�LB	�LB	�RB	�RB	�RB	�XB	�XB	�dB	�dB	�jB	�}B	��B	��B	��B	��B	ÖB	ÖB	ĜB	ŢB	ÖB	ŢB	ŢB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�)B	�;B	�HB	�HB	�HB	�NB	�ZB	�mB	�mB	�fB	�`B	�`B	�fB	�`B	�TB	�TB	�HB	�NB	�sB	�sB	�fB	�`B	�fB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
�B
 �B
+B
2-B
8RB
=qB
A�B
B�B
J�B
S�B
ZB
_;B
cTB
hsB
m�B
q�B
t�B
x�B
z�B
~�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B��B��B��B��B�~B�~B�|B�BXB(�B=)BQ�BoXB��B�mB��B� B��B�IB�gB�BB |B'�B0�B=+BJwBV�BY�B[�BdBf!Bh+Bh'Bh*Bj7Bj;Bj?Bh.Bg'BeB]�BU�BO�BBHB1�B-�B+�B*�B%�BWB<B9BB�B��B��BΓB�}B�oB�\B�xB��B�RB�aB��B�B�B��B��B��B�LB�B}�Bp_Bg%BM�BEVB=*B.�B!�BBB
�B�B�~B�TB�/B��BΑB�sB�B��B��B{�BuxBoVBlBBfBY�BK�B=%B1�BZB�B
�gB
��B
΍B
�lB
�B
�uB
�B
|�B
nOB
^�B
P�B
0�B
.B	�8B	ҦB	�0B	��B	��B	�dB	��B	uuB	_�B	O�B	@7B	8B	0�B	%�B	`B	!B	�B	�B��B��B�|B�`B�@B�B��BӪB̈́B�iB�[B�IB�6B�#B�B�B��B��B��B��B��B��B��B�vB�jB�fB�gB�YB�NB�DB�TB�LB�JB�?B�AB�AB�AB�HB�FB�HB�?B�?B�>B�?B�GB�YB�hB�jB�cB�cB�fB�dB�RB�AB�5B�/B�(B�(B�B� B�B�B�B�B�B� B�B��B��B�B�B�B�\B�WB�HB�HB�dB��B��B��B��B�}B�kB�rB�dB�RB�:B�B��B��B��B��B}�B�B��B��B��B��B��B��B��B��B��B��B� B�B��B�B�B�B�&B�7B�;B�GB�FB�KB�BB�GB�EB�?B�CB�?B�<B�8B�>B�XB�kB�jB�lB�}B��B��B��B��B��B��B��B��B��B��B�B�B�B�/B�CB�\B̀B΋BӨB��B��B��B��B�
B�	B�B�B�B�*B�GB�TB�UB�WB�XB�YB�XB�pB��B��B��B	�B	�B	�B		�B	B	B	-B	EB	YB	pB	!�B	"�B	'�B	+�B	,�B	-�B	/�B	0�B	1�B	3�B	5�B	8B	:B	?-B	?1B	BAB	GcB	InB	JtB	JtB	O�B	Q�B	R�B	R�B	S�B	T�B	U�B	W�B	[�B	\�B	]�B	^�B	^�B	`�B	eB	h%B	i,B	k9B	mEB	p^B	skB	w�B	x�B	x�B	y�B	z�B	|�B	~�B	�B	��B	��B	��B	��B	��B	��B	�B	�B	�*B	�OB	�SB	�OB	�^B	�yB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�&B	�5B	�>B	�>B	�<B	�AB	�QB	�QB	�VB	�[B	�PB	�^B	�]B	�cB	�jB	�vB	̂B	͊B	ҧB	ҧB	ӮB	ӭB	չB	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�
B	�B	�%B	�&B	�B	�B	�B	�B	�B	�B	�B	� B	�	B	�,B	�,B	�B	�B	�B	�?B	�eB	�_B	�\B	�MB	�WB	�[B	�WB	�ZB	�VB	�SB	�MB	�NB	�FB	�JB	�RB	�MB	�[B	�dB	�cB	�lB	�jB	�wB	�wB	��B	��B	��B	��B	��B	��B	��B	��B	��B
�B
>B
!�B
+�B
2�B
9
B
>/B
BBB
CHB
KyB
T�B
Z�B
_�B
dB
i-B
nNB
rgB
uxB
y�B
{�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.06 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =1(+/-0), vertically averaged dS =0.001(+/-0.001) in PSS-78.                                                                                                                                                                                                  Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Map scales: x=6,3; y=2,1. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                            201906040939592019060409395920190604093959  AO  ARCAADJP                                                                    20160114101556    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160114101556  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160114101556  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604093959  IP                  G�O�G�O�G�O�                