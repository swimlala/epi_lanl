CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-18T14:13:50Z creation      
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
_FillValue                 �  A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  K$   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \X   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^D   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  e�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  g�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ox   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �<   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �l   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �l   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �l   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �l   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20200618141350  20220204114415  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               3A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @؝ծ�(`1   @؝�8�@@5Ұ ě��cԬ1&�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    3A   B   B   @�ff@�  A   A   A@  Aa��A�  A�33A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�33B�33B�  B�  B�  B�  B���B���B���B�  B�  B�  B�33B�33B���B���B�  B�  B�  B�  B�  B���B���B���B�  B�  B�  B�33B�  B���B���C   C  C�C�C  C
  C�C  C  C�C  C  C�C  C�fC  C �C"�C$  C&  C(  C*  C,  C.  C0  C2  C4  C6�C8�C:  C;�fC>  C@  CB  CD�CF  CH  CJ  CL  CM�fCP  CR  CT  CV  CX�CZ  C\  C]�fC_�fCb  Cd  Ce�fCh  Cj�Cl  Cm�fCp  Cr  Ct  Cv  Cx  Cz  C|  C~  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C�  C�  C�  C�  C��C��C�  C��3C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C��3C�  C�  C�  C�  C��C��C��3C��3C�  C�  C�  C��C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C�  C��C�  C��3C�  C�  C��C��C�  C��C��C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C��C�  C�  C�  C��3C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C�  C�  C�  D fD � D ��D� D  D�fDfD� D  Dy�D  D� D  D�fD  D� D  D� D	fD	�fD
fD
�fDfD�fDfD�fD  D� D  Dy�D  D�fD  D� D��D� D  D� D  D� D  D� D  Dy�D  D�fDfD� D  Dy�D��D� DfD�fD  D� D  Dy�D��D� DfD� D��D� D   D � D ��D!� D"fD"� D#  D#�fD$fD$� D$��D%� D&fD&� D'  D'� D(fD(�fD)  D)y�D*  D*�fD+  D+� D,  D,� D,��D-� D.fD.� D/  D/� D/��D0� D1  D1� D1��D2� D3fD3�fD4fD4� D5  D5� D6  D6� D7  D7�fD8  D8� D9  D9�fD:  D:� D;fD;y�D;��D<� D=  D=� D>  D>� D?  D?� D@  D@�fDA  DA� DBfDB� DC  DC�fDD  DD� DEfDE� DF  DF� DG  DG� DHfDH� DI  DI�fDJ  DJy�DJ��DK� DL  DL� DM  DM�fDN  DN� DO  DO� DO��DP� DQ  DQy�DR  DR� DR��DS� DTfDT� DU  DU�fDV  DV� DW  DW�fDX  DX� DYfDY� DY��DZ� D[fD[� D\  D\y�D\��D]� D^  D^y�D^��D_� D`fD`�fDa  Da�fDbfDb�fDcfDc�fDdfDd�fDefDe� Df  Df� DgfDg�fDhfDh�fDifDi� Di��Djy�Dk  Dk� Dk��Dl� DmfDm�fDn  Dny�Do  Do�fDp  Dp� DqfDq� DqٚDx��D��)D���D�3D�ED��R111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�z�@�{@�{A
=A?
=A`��A
=A��RA��A��A��AυA߅A�A��BBBBB'B/B7B?BGBOBWB_BgBoBwBB�{B�{B��HB��HB��HB��HB��B��B��B��HB��HB��HB�{B�{B��B��B��HB��HB��HB��HB��HB׮BۮB߮B��HB��HB��HB�{B��HB��B��B��HC�C
>C
>C�C	�C
>C�C�C
>C�C�C
>C�C�
C�C 
>C"
>C#�C%�C'�C)�C+�C-�C/�C1�C3�C6
>C8
>C9�C;�
C=�C?�CA�CD
>CE�CG�CI�CK�CM�
CO�CQ�CS�CU�CX
>CY�C[�C]�
C_�
Ca�Cc�Ce�
Cg�Cj
>Ck�Cm�
Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C�C��RC��RC��RC��RC��RC��RC��RC��RC��RC�C��RC��C��RC��RC��RC��RC��RC��RC��RC��RC��RC�C��RC��RC�C��RC��RC��RC��RC�C�C��RC��C��RC��RC�C�C��RC��RC��RC��RC��RC��RC��RC��C��C��RC��RC��RC��C��RC��RC��RC��RC�C�C��C��C��RC��RC��RC�C�C��RC��RC�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��C��RC��RC��C��RC�C��RC��C��RC��RC�C�C��RC�C�C��RC��RC��RC�C��RC��C��RC��RC��RC��RC�C��RC��RC��RC��C��RC��RC��C��RC�C��RC��RC��RC��RC��RC��RC��RC�C��RC��RC�C��RC��RC��RD �D |)D ��D|)D�)D��D�D|)D�)Du�D�)D|)D�)D��D�)D|)D�)D|)D	�D	��D
�D
��D�D��D�D��D�)D|)D�)Du�D�)D��D�)D|)D��D|)D�)D|)D�)D|)D�)D|)D�)Du�D�)D��D�D|)D�)Du�D��D|)D�D��D�)D|)D�)Du�D��D|)D�D|)D��D|)D�)D |)D ��D!|)D"�D"|)D"�)D#��D$�D$|)D$��D%|)D&�D&|)D&�)D'|)D(�D(��D(�)D)u�D)�)D*��D*�)D+|)D+�)D,|)D,��D-|)D.�D.|)D.�)D/|)D/��D0|)D0�)D1|)D1��D2|)D3�D3��D4�D4|)D4�)D5|)D5�)D6|)D6�)D7��D7�)D8|)D8�)D9��D9�)D:|)D;�D;u�D;��D<|)D<�)D=|)D=�)D>|)D>�)D?|)D?�)D@��D@�)DA|)DB�DB|)DB�)DC��DC�)DD|)DE�DE|)DE�)DF|)DF�)DG|)DH�DH|)DH�)DI��DI�)DJu�DJ��DK|)DK�)DL|)DL�)DM��DM�)DN|)DN�)DO|)DO��DP|)DP�)DQu�DQ�)DR|)DR��DS|)DT�DT|)DT�)DU��DU�)DV|)DV�)DW��DW�)DX|)DY�DY|)DY��DZ|)D[�D[|)D[�)D\u�D\��D]|)D]�)D^u�D^��D_|)D`�D`��D`�)Da��Db�Db��Dc�Dc��Dd�Dd��De�De|)De�)Df|)Dg�Dg��Dh�Dh��Di�Di|)Di��Dju�Dj�)Dk|)Dk��Dl|)Dm�Dm��Dm�)Dnu�Dn�)Do��Do�)Dp|)Dq�Dq|)Dq��Dx~D��=D���D�	GD�C3D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�v�A�t�A�z�A�r�A�p�AȅAȍPAȍPAȉ7AȋDAȋDA�ffA�-A���A�~�A�jA�VA�AƩ�A�v�A�E�A���A�t�A�A���Aď\A�K�AîA��A§�A�A§�A�z�A�`BA�G�A��A��yA�A�x�A�{A�ĜA�hsA�A�t�A�-A��A�\)A�C�A�VA���A�^5A��-A��A�x�A�1'A��-A���A�;dA���A�=qA�ĜA�jA��\A��#A�VA�ƨA���A��#A��A��DA��A��9A��7A�^5A��A�XA�+A���A��
A�=qA���A�;dA��TA�A���A�7LA��mA���A��mA��yA�v�A���A�dZA��HA��^A�K�A�ĜA���A�O�A�;dA��A�  A���A�M�A��A��jA��A��A�?}A���A���A�/A�A�A���A�\)A�  A���A�n�A��Al�AA}�FAx�Au\)Au33At �AqAnz�AmhsAk�Ai��AgC�Af  Ae��Ae�AdJAc?}Abn�A`�/A`9XA[�FAVĜAV�DAVv�AU�AS�wARI�AP�AKp�AG?}AD�ABĜA@��A?��A=�A;\)A8��A6�!A6�\A61'A4ĜA45?A2Q�A0~�A.(�A,�uA)�-A'/A$��A#�PA#
=A!hsA �A9XA\)A%A�;At�A|�A�mA�\A|�A?}AJA�yAK�Az�A�A��A-A`BA+AoA��A�RAXA^5A�A�wAjA �`A ��A n�A 9X@���@���@�|�@��@�(�@��;@�|�@�
=@�5?@���@��m@�"�@��9@��`@�u@���@ꟾ@�p�@�F@��y@�E�@�z�@�n�@���@�1'@ާ�@�~�@�^5@�E�@��@��T@��T@��#@���@���@�@�x�@�?}@ܴ9@�|�@��H@ו�@ӕ�@��@���@��@̋D@�Z@�b@��;@˾w@˝�@�"�@ʟ�@��@�1@Ǿw@�"�@�ff@�=q@Ł@�%@�9X@�33@�$�@���@��u@�j@�1@�S�@��H@�~�@�{@���@��7@�Ĝ@�I�@�  @��@��@��\@�X@�r�@�1'@��@��@�E�@���@��@�%@���@�z�@�|�@�n�@�J@��h@��7@�t�@�r�@���@�|�@���@�ȴ@���@��+@�E�@�$�@�bN@�M�@�=q@�5?@�-@���@��^@�`B@���@�Ĝ@���@��D@�Z@�b@�;d@�G�@�A�@��
@�K�@���@���@�?}@��@�j@�o@��R@�~�@��@��`@���@���@��;@�;d@���@��\@��@��-@���@�z�@�bN@�Z@�Q�@�A�@�Q�@�bN@�z�@�r�@�Q�@�A�@�9X@�9X@�9X@�A�@�A�@�A�@�I�@�A�@�9X@�9X@�9X@�1'@�(�@�(�@��@��@�?}@�%@��@��`@��`@��D@���@�+@��@���@�5?@��T@���@���@��7@�p�@�`B@�G�@�&�@��`@��@�S�@��+@���@��@��@��@��@���@���@�  @�1@�1@��@� �@�(�@� �@�b@���@�ƨ@�dZ@��@�ȴ@��R@��+@��@��-@���@��7@�?}@��@�r�@��
@���@�K�@�
=@�@�o@�o@�
=@�
=@��y@���@��!@��!@��R@��!@�v�@�{@��@���@���@��@�O�@�?}@��@��@���@��j@���@�z�@�Q�@�1@��@���@��F@��w@��F@�S�@�o@���@��H@�ȴ@��!@���@���@��\@�v�@�v�@�n�@�^5@�E�@�E�@�5?@�J@��@��@�oi@w_p@uu�@h�@c�@Q�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111   A�v�A�t�A�z�A�r�A�p�AȅAȍPAȍPAȉ7AȋDAȋDA�ffA�-A���A�~�A�jA�VA�AƩ�A�v�A�E�A���A�t�A�A���Aď\A�K�AîA��A§�A�A§�A�z�A�`BA�G�A��A��yA�A�x�A�{A�ĜA�hsA�A�t�A�-A��A�\)A�C�A�VA���A�^5A��-A��A�x�A�1'A��-A���A�;dA���A�=qA�ĜA�jA��\A��#A�VA�ƨA���A��#A��A��DA��A��9A��7A�^5A��A�XA�+A���A��
A�=qA���A�;dA��TA�A���A�7LA��mA���A��mA��yA�v�A���A�dZA��HA��^A�K�A�ĜA���A�O�A�;dA��A�  A���A�M�A��A��jA��A��A�?}A���A���A�/A�A�A���A�\)A�  A���A�n�A��Al�AA}�FAx�Au\)Au33At �AqAnz�AmhsAk�Ai��AgC�Af  Ae��Ae�AdJAc?}Abn�A`�/A`9XA[�FAVĜAV�DAVv�AU�AS�wARI�AP�AKp�AG?}AD�ABĜA@��A?��A=�A;\)A8��A6�!A6�\A61'A4ĜA45?A2Q�A0~�A.(�A,�uA)�-A'/A$��A#�PA#
=A!hsA �A9XA\)A%A�;At�A|�A�mA�\A|�A?}AJA�yAK�Az�A�A��A-A`BA+AoA��A�RAXA^5A�A�wAjA �`A ��A n�A 9X@���@���@�|�@��@�(�@��;@�|�@�
=@�5?@���@��m@�"�@��9@��`@�u@���@ꟾ@�p�@�F@��y@�E�@�z�@�n�@���@�1'@ާ�@�~�@�^5@�E�@��@��T@��T@��#@���@���@�@�x�@�?}@ܴ9@�|�@��H@ו�@ӕ�@��@���@��@̋D@�Z@�b@��;@˾w@˝�@�"�@ʟ�@��@�1@Ǿw@�"�@�ff@�=q@Ł@�%@�9X@�33@�$�@���@��u@�j@�1@�S�@��H@�~�@�{@���@��7@�Ĝ@�I�@�  @��@��@��\@�X@�r�@�1'@��@��@�E�@���@��@�%@���@�z�@�|�@�n�@�J@��h@��7@�t�@�r�@���@�|�@���@�ȴ@���@��+@�E�@�$�@�bN@�M�@�=q@�5?@�-@���@��^@�`B@���@�Ĝ@���@��D@�Z@�b@�;d@�G�@�A�@��
@�K�@���@���@�?}@��@�j@�o@��R@�~�@��@��`@���@���@��;@�;d@���@��\@��@��-@���@�z�@�bN@�Z@�Q�@�A�@�Q�@�bN@�z�@�r�@�Q�@�A�@�9X@�9X@�9X@�A�@�A�@�A�@�I�@�A�@�9X@�9X@�9X@�1'@�(�@�(�@��@��@�?}@�%@��@��`@��`@��D@���@�+@��@���@�5?@��T@���@���@��7@�p�@�`B@�G�@�&�@��`@��@�S�@��+@���@��@��@��@��@���@���@�  @�1@�1@��@� �@�(�@� �@�b@���@�ƨ@�dZ@��@�ȴ@��R@��+@��@��-@���@��7@�?}@��@�r�@��
@���@�K�@�
=@�@�o@�o@�
=@�
=@��y@���@��!@��!@��R@��!@�v�@�{@��@���@���@��@�O�@�?}@��@��@���@��j@���@�z�@�Q�@�1@��@���@��F@��w@��F@�S�@�o@���@��H@�ȴ@��!@���@���@��\@�v�@�v�@�n�@�^5@�E�@�E�@�5?@�J@��G�O�@�oi@w_p@uu�@h�@c�@Q�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;oB^5B^5B^5B^5B^5B]/B^5B^5B]/B]/B]/BbNBgmBo�Bu�Bt�Bw�B�+B�{B��B��B�9B�wB��B��B�HB�BoB/B5?B6FB=qBJ�BH�BI�BJ�BJ�BJ�BJ�BI�BF�BJ�BM�BW
BW
BZB`BBz�Bv�Bx�Bv�Bv�Bm�B�B�Bz�Bx�B{�Bx�B{�Bz�B{�B�+B�B�JB�B�VB�bBv�Bv�Bu�Bp�Bm�Bk�Bm�Be`B^5BP�BJ�B:^B6FB6FB&�B)�B�B+B��B��B�yB�}B�9B�B��B��B��B��B�1B�B~�B|�BcTB@�B,B$�B�BuB\B1B
��B
��B
��B
�!B
��B
��B
�hB
�JB
�1B
�B
~�B
v�B
q�B
m�B
^5B
.B
(�B
'�B
�B
B	�B	�B	�BB	��B	��B	�dB	�dB	�FB	��B	��B	��B	�bB	��B	R�B	L�B	I�B	J�B	=qB	(�B	(�B	
=B��B�;B�
B��B��BƨB�-B�?B��B��B��B��B�DB�\B� B� Br�Bu�BiyBhsB[#BXB^5BR�BZBK�BJ�BM�BM�BI�BC�BA�B=qBA�B5?B8RB7LB5?B5?B6FB0!B0!B.B-B,B-B2-B-B+B(�B7LB/B)�B,B+B.B+B-B.B(�B(�B)�B(�B+B)�B'�B&�B.B49B5?B.B-B0!B0!B/B-B33B6FB1'B6FB5?B33B33B33B49B33B33B33B33B2-B2-B2-B1'B33B2-B/B?}B<jB9XB:^BG�BF�BD�BF�BE�BD�BD�BE�BG�BI�BJ�BK�BM�BK�BK�BM�BK�BN�BR�BW
BZB[#B[#B]/B`BBaHBcTBdZBgmBe`BjBm�Bn�Bs�Bv�B|�B�B�B�B�%B�%B�1B�7B�DB�JB�DB�PB�bB��B��B�B�jBĜB�B�#B�B�B�B��B��B��B��B�NB�`B�NB�NB�NB�ZB�ZB�`B�`B�`B�`B�`B�`B�`B�yB�B�B�B�yB�B�B�B�B�B�B�B�B��B��B��B��B��B	  B	B	B	B	+B	bB	hB	oB	oB	uB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	"�B	;dB	=qB	>wB	?}B	>wB	>wB	B�B	J�B	L�B	M�B	P�B	T�B	XB	XB	ZB	ZB	[#B	[#B	\)B	\)B	]/B	e`B	jB	r�B	y�B	�B	�DB	�DB	�JB	�PB	�PB	�VB	�\B	�bB	�bB	�hB	�uB	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�?B	�LB	�FB	�RB	�XB	�^B	�wB	��B	��B	ÖB	ĜB	ĜB	ĜB	ŢB	ŢB	ŢB	ƨB	ŢB	ƨB	ƨB	ƨB	ƨB	ǮB	ǮB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�)B	�/B	�/B	�/B	�5B	�5B	�5B	�5B	�;B	�;B	�;B	�5B	�;B	�;B	�5B	�5B	�BB	�;B	�5B	��B
tB
B
B
�B
0�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111   BU�BU�BU�BU�BU�BT�BU�BU�BT�BT�BT�BZB_5BgfBm�Bl�Bo�B~�B�@B�kB��B��B�:BǡB��B�	B�dB
-B&�B,�B.B5-BB|B@oBAuBB|BB|BB|BB|BAuB>cBB|BE�BN�BN�BQ�BW�Br�Bn�Bp�Bn�Bn�BeLBx�By�Br�Bp�Bs�Bp�Bs�Br�Bs�B~�B{�B�By�B�B�Bn�Bn�BmBhaBeNBcBBeNB]BU�BH�BB�B2B.B.B�B!�BKB��B�B�B�@B�GB�B��B��B�mB�TB�ZB�B|�Bv�Bt�B[%B8WB#�B�BiBLB3B 	B
��B
��B
ǻB
��B
��B
�aB
�IB
�+B
�B
y�B
v�B
n�B
i�B
etB
VB
%�B
 �B
�B
�B	�
B	�B	�B	�0B	��B	�yB	�TB	�TB	�7B	��B	��B	��B	�UB	��B	J�B	D�B	A�B	B�B	5kB	 �B	 �B	;B��B�<B�B��B��B��B�2B�DB��B��B��B��B�LB�dBx	Bx	Bj�Bm�Ba�B`BS0BPBVBBJ�BR*BC�BB�BE�BE�BA�B;�B9�B5�B9�B-PB0cB/]B-PB-QB.XB(3B(3B&&B%!B$B%!B*?B%!B#B!	B/^B'.B"B$B#B&'B#B%!B&(B!
B!
B"B!
B#B"B B�B&(B,MB-SB&)B%#B(6B(6B'0B%#B+HB.[B)<B.[B-TB+HB+HB+HB,NB+HB+HB+HB+HB*BB*BB*BB)<B+HB*BB'0B7�B4B1mB2sB?�B>�B<�B>�B=�B<�B<�B=�B?�BA�BB�BC�BE�BC�BC�BE�BC�BF�BKBOBR1BS7BS7BUCBXVBY\B[hB\nB_�B]tBb�Be�Bf�Bk�Bn�BuB|+B|+B|+B~7B~7B�CB�IB�VB�\B�VB�bB�tB��B��B�B�yB��B�B�0B�B�B�B�B�B�B�B�[B�mB�[B�[B�[B�gB�gB�mB�mB�mB�mB�mB�mB�mB�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B�B�B�B�+B�7B	nB		tB	
{B	
{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	3mB	5zB	6�B	7�B	6�B	6�B	:�B	B�B	D�B	E�B	H�B	MB	PB	PB	R%B	R%B	S+B	S+B	T1B	T1B	U7B	]gB	b�B	j�B	q�B	|B	�IB	�IB	�OB	�UB	�UB	�[B	�aB	�gB	�gB	�mB	�zB	��B	��B	��B	��B	��B	�B	�B	�B	�$B	�BB	�OB	�IB	�TB	�ZB	�`B	�yB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�*B	�0B	�0B	�0B	�6B	�6B	�6B	�6B	�<B	�<B	�<B	�6B	�<B	�<B	�6B	�6B	�CB	�<G�O�B	��B	�sB
 B
}B
�B
(�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.06 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.008(+/-0.003) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144152022020411441520220204114415  AO  ARCAADJP                                                                    20200618141350    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200618141350  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200618141350  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114415  IP                  G�O�G�O�G�O�                