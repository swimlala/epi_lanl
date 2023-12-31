CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:09:23Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200619170923  20220204114423  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               }A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @��[�:�1   @��\�8�@7-V�b�bM��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    }A   B   B   @���@�  @���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  A�33B��B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C�C�C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dky�Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDy��D�$)D�\�D���D�ȤD�	�D�Z=D��=D��)D��D�^D���D�ڏD�'
D�YHDڋ3D���D�HD�W
D��D��
111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @\*@�{@��HA
=A;
=A[
=A{
=A��A��A��A��AͅA݅A�A��RB\)BBBB&B.B6B>BFBNBVB^BfBnBvB~B�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHBה{B�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHC��C��C��C��C	��C��C��C��C��C�>C�>C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RD l)D �)Dl)D�)Dl)D�)Dl)D�)Dl)D�)De�D�)Dl)D�)Dl)D�)Dl)D�)D	l)D	�)D
l)D
�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D��Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)D l)D �)D!l)D!�)D"l)D"�)D#l)D#�)D$l)D$�)D%l)D%�)D&l)D&�)D'l)D'�)D(l)D(�)D)l)D)�)D*l)D*�)D+l)D+�)D,l)D,�)D-l)D-�)D.l)D.�)D/l)D/�)D0l)D0�)D1l)D1�)D2l)D2�)D3l)D3�)D4l)D4�)D5l)D5�)D6l)D6�)D7l)D7�)D8l)D8�)D9l)D9�)D:l)D:�)D;l)D;�)D<l)D<�)D=l)D=�)D>l)D>�)D?l)D?�)D@l)D@�)DAl)DA�)DBl)DB�)DCl)DC�)DDl)DD�)DEl)DE�)DFl)DF�)DGl)DG�)DHl)DH�)DIl)DI�)DJl)DJ�)DKl)DK�)DLl)DL�)DMl)DM�)DNl)DN�)DOl)DO�)DPl)DP�)DQl)DQ�)DRl)DR�)DSl)DS�)DTl)DT�)DUl)DU�)DVl)DV�)DWl)DW�)DXl)DX�)DYl)DY�)DZl)DZ�)D[l)D[�)D\l)D\�)D]l)D]�)D^l)D^�)D_l)D_�)D`l)D`�)Dal)Da�)Dbl)Db�)Dcl)Dc�)Ddl)Dd�)Del)De�)Dfl)Df�)Dgl)Dg�)Dhl)Dh�)Dil)Di�)Djl)Dj�)Dke�Dk�)Dll)Dl�)Dml)Dm�)Dnl)Dn�)Dol)Do�)Dpl)Dp�)Dql)Dq�)Drl)Dr�)Dsl)Ds�)Dtl)DtҏDy��D�=D�R�D���D���D�  D�PQD��QD��=D��D�T(D���D�УD�D�O\DځGD���D�\D�MD��D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�A�A�C�A�?}A�?}A�K�A�O�A�O�A�Q�A�S�A�S�A�S�A�S�A�VA�VA�VA�VA�XA�ZA�ZA�ZA�ZA�ZA�\)A�ZA�VA�ZA�^5A�bNA�bNA�hsA�jA�n�A�n�A�n�A�p�A�r�A�r�A�t�A�n�A�r�A�n�A�l�A�jA�XA�ĜAú^A�;dA�{A��A��HA¾wA�A�p�A��A��A��;A�%A�O�A��A���A��A�1A��jA�|�A��#A��^A���A���A�7LA��9A�"�A���A��PA��#A�K�A�^5A�"�A�`BA���A���A���A��+A�ȴA�1'A��A�M�A��-A�/A�C�A���A�-A���A�dZA���A���A��wA�t�A���A��`A���A��A���A��yA�1A��A�"�A���A��A�{A���A�%A���A�1'A��!A���A��A��9A�\)A�^A~�/A~�A|ffAy�Ax(�Au��AqAm|�Al-Ai��Ag�Ac�A_C�A\�uA[33AWVASS�AQ�TAP��AP{AN�uAL{AJ��AH1AF�+AD5?AB�\AAAA?}A@bNA?��A?&�A=
=A:�jA9�A9A7�mA6bNA4�A4bNA3hsA2�+A1�A1�A0(�A/`BA.�RA.  A,��A+��A*��A*A�A)�A)��A(��A'?}A%t�A#�A#x�A#%A"5?A!;dA �A��A��A��A�DA�FA�uA��A��A7LAn�Al�A�RA�A�A��A��Ax�AJAVAl�A��AA�A-A��AC�A�+AVA�A�PA�DA�jA�A��A
�A
=qA	A	�FA�#A�A��A�A��Ap�A  A�hA ��@�t�@���@��@��y@��@�=q@�I�@��@�z�@�1'@�(�@�@�/@�/@���@�I�@�$�@�1'@�@���@�^5@�G�@�|�@�  @�n�@�J@��@�r�@�|�@���@��@ץ�@ְ!@�I�@��@�$�@ѡ�@�/@�G�@ҟ�@�V@���@��@ͩ�@���@�@̃@��@�b@Ƨ�@��@�`B@�Ĝ@î@��@�`B@�V@�%@���@���@�Q�@�(�@�o@���@��j@��@��@�"�@�
=@��R@�{@�X@�%@���@�  @���@��@�Z@��;@�t�@�o@���@��y@�@�o@�o@���@�E�@��#@��^@��#@���@��\@�v�@�-@�r�@�1'@��@�j@�1'@���@�@�1@�  @�1'@���@�?}@��@�V@���@���@�(�@��;@�Z@�ƨ@�ȴ@��@�@�@���@���@�{@��h@�/@�%@�G�@�7L@���@�@�{@���@�x�@��j@��@�
=@���@�5?@���@���@��@�`B@�x�@��@�p�@�/@��u@�(�@��@�b@��H@�O�@�z�@� �@��@���@��@��@�33@�ȴ@�n�@�ff@�{@�x�@�7L@�&�@��@��@�Z@�I�@�b@���@�t�@�\)@���@��@��H@��@���@�=q@�{@�@���@��T@��#@���@�@��7@�x�@�X@�&�@���@��@���@��9@��@��D@�Q�@�1'@��
@�dZ@���@�n�@��+@���@��\@�~�@�v�@�^5@��@��@��7@�V@���@��j@��9@���@��j@�bN@��@��@���@��m@�ƨ@��P@�+@���@��\@��@��!@�E�@�@���@�hs@���@���@�b@��;@���@�C�@�33@�
=@���@��!@��+@���@���@��\@�~�@�5?@�$�@�{@���@�/@���@���@�j@�1'@��m@�|�@�+@��y@���@y[W@q��@j��@e:�@\1'@U8�@Nv�@J��@C��@<b@5�@.��@)��@$��@�6@#�@<�@خ@�@N�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�A�A�C�A�?}A�?}A�K�A�O�A�O�A�Q�A�S�A�S�A�S�A�S�A�VA�VA�VA�VA�XA�ZA�ZA�ZA�ZA�ZA�\)A�ZA�VA�ZA�^5A�bNA�bNA�hsA�jA�n�A�n�A�n�A�p�A�r�A�r�A�t�A�n�A�r�A�n�A�l�A�jA�XA�ĜAú^A�;dA�{A��A��HA¾wA�A�p�A��A��A��;A�%A�O�A��A���A��A�1A��jA�|�A��#A��^A���A���A�7LA��9A�"�A���A��PA��#A�K�A�^5A�"�A�`BA���A���A���A��+A�ȴA�1'A��A�M�A��-A�/A�C�A���A�-A���A�dZA���A���A��wA�t�A���A��`A���A��A���A��yA�1A��A�"�A���A��A�{A���A�%A���A�1'A��!A���A��A��9A�\)A�^A~�/A~�A|ffAy�Ax(�Au��AqAm|�Al-Ai��Ag�Ac�A_C�A\�uA[33AWVASS�AQ�TAP��AP{AN�uAL{AJ��AH1AF�+AD5?AB�\AAAA?}A@bNA?��A?&�A=
=A:�jA9�A9A7�mA6bNA4�A4bNA3hsA2�+A1�A1�A0(�A/`BA.�RA.  A,��A+��A*��A*A�A)�A)��A(��A'?}A%t�A#�A#x�A#%A"5?A!;dA �A��A��A��A�DA�FA�uA��A��A7LAn�Al�A�RA�A�A��A��Ax�AJAVAl�A��AA�A-A��AC�A�+AVA�A�PA�DA�jA�A��A
�A
=qA	A	�FA�#A�A��A�A��Ap�A  A�hA ��@�t�@���@��@��y@��@�=q@�I�@��@�z�@�1'@�(�@�@�/@�/@���@�I�@�$�@�1'@�@���@�^5@�G�@�|�@�  @�n�@�J@��@�r�@�|�@���@��@ץ�@ְ!@�I�@��@�$�@ѡ�@�/@�G�@ҟ�@�V@���@��@ͩ�@���@�@̃@��@�b@Ƨ�@��@�`B@�Ĝ@î@��@�`B@�V@�%@���@���@�Q�@�(�@�o@���@��j@��@��@�"�@�
=@��R@�{@�X@�%@���@�  @���@��@�Z@��;@�t�@�o@���@��y@�@�o@�o@���@�E�@��#@��^@��#@���@��\@�v�@�-@�r�@�1'@��@�j@�1'@���@�@�1@�  @�1'@���@�?}@��@�V@���@���@�(�@��;@�Z@�ƨ@�ȴ@��@�@�@���@���@�{@��h@�/@�%@�G�@�7L@���@�@�{@���@�x�@��j@��@�
=@���@�5?@���@���@��@�`B@�x�@��@�p�@�/@��u@�(�@��@�b@��H@�O�@�z�@� �@��@���@��@��@�33@�ȴ@�n�@�ff@�{@�x�@�7L@�&�@��@��@�Z@�I�@�b@���@�t�@�\)@���@��@��H@��@���@�=q@�{@�@���@��T@��#@���@�@��7@�x�@�X@�&�@���@��@���@��9@��@��D@�Q�@�1'@��
@�dZ@���@�n�@��+@���@��\@�~�@�v�@�^5@��@��@��7@�V@���@��j@��9@���@��j@�bN@��@��@���@��m@�ƨ@��P@�+@���@��\@��@��!@�E�@�@���@�hs@���@���@�b@��;@���@�C�@�33@�
=@���@��!@��+@���@���@��\@�~�@�5?@�$�@�{@���@�/@���@���@�j@�1'@��m@�|�@�+G�O�@���@y[W@q��@j��@e:�@\1'@U8�@Nv�@J��@C��@<b@5�@.��@)��@$��@�6@#�@<�@خ@�@N�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B�BbNBhsBo�Bu�Bu�Bt�B|�B�7B��B��B�B�^BȴB�}B�qB�/B  BPBbB�B%�B.B2-B$�B!�B&�B �B+B-B49B0!B/B�B�B�B#�B&�BJBJB1B�B��B��B�Bv�B�7B�\B�\BǮB��B�'B�B��B��B�=B�Bx�B]/BbNBP�B<jB0!B
��B
��B
��B
��B
�JB
A�B
$�B
�B
B	��B	��B	�B	��B	��B	�B	�5B	�B	ɺB	�B	�=B	}�B	k�B	W
B	A�B	6FB	#�B	�B	PB��B�B�B�mB�`B�#B�B��BȴB��B�RB�9B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�bB�PB�DB�1B�B�B� B~�B}�B{�Bx�Bs�Bn�Bm�Bp�Bm�Bk�BffBdZBdZBbNB`BB_;B\)BYBXBYBXBW
BW
BaHB�=B�{B��B��B�wBÖB�wB�FB��B��B�uB�Bz�Bq�B_;BXBS�B]/Be`BbNB`BB_;B`BBt�Bo�Bm�Bk�BhsBe`B^5BT�BQ�BQ�BO�BK�BE�BK�BVBZBYB[#BW
BVBO�BH�BN�BZB[#BaHB^5BdZBdZBe`Be`BdZB`BB]/B[#B_;BaHB_;BYBXBS�BZB_;B]/BbNBbNBiyBm�Bs�B�B�B}�Bx�By�B�B�B�B}�Bw�Bv�Bt�Bv�Bv�Bx�Bw�Bx�By�B~�B~�B�B�B�%B�=B�JB�PB�DB�=B�7B�=B�JB�JB�PB�PB�bB�uB��B��B��B��B��B��B��B�B�9B�FB�XB�^B�XB�qBĜBɺB��B��B�B�5B�5B�BB�fB�B�B�B�B�B�B��B�B�B�yB�B�B�B��B��B��B	  B��B	B		7B	DB	JB	\B	hB	oB	uB	{B	�B	�B	$�B	&�B	0!B	1'B	1'B	49B	49B	33B	2-B	6FB	?}B	B�B	B�B	A�B	B�B	C�B	G�B	H�B	H�B	I�B	J�B	J�B	J�B	H�B	F�B	I�B	J�B	L�B	M�B	N�B	O�B	O�B	O�B	P�B	R�B	Q�B	Q�B	R�B	S�B	VB	VB	YB	\)B	`BB	aHB	aHB	cTB	ffB	hsB	jB	l�B	k�B	n�B	p�B	w�B	x�B	y�B	z�B	|�B	�B	�B	�+B	�DB	�PB	�PB	�VB	�bB	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�9B	�FB	�LB	�XB	�^B	�qB	�qB	�wB	��B	ĜB	ĜB	ƨB	ǮB	ȴB	ǮB	ǮB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�B	�#B	�#B	�)B	�/B	�/B	�5B	�5B	�BB	�NB	�TB	�6B	��B
�B
�B
$�B
*�B
7B
>]B
C�B
I�B
M�B
P�B
SB
YB
c�B
fLB
i�B
l�B
p�B
u�B
zx111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
�SB
�RB
�RB
�RB
�RB
�RB
�RB
�RB
�RB
�RB
�RB
�RB
�RB
�RB
�RB
�RB
�RB
�RB
�RB
�RB
�RB
�RB
�RB
�RB
�RB
�RB
�RB
�RB
�RB
�RB
�RB
�RB
�RB
�RB
�RB
�RB
�RB
�RB
�RB
�RB
�RB
�YB
�YB
�wBiBVB\*BcTBixBixBhrBp�B|�B�RB�pB��B�B�cB�-B�"B��B�B �BBgB�B!�B%�B�BuB�BpB�B �B'�B#�B"�BTBHBfB�B�B��B��B��B�QBĜB�nBv�Bj�B|�B�B�B�gB�CB��B��B��B�^B}�Bx�Bl�BP�BVBD�B03B#�B
��B
��B
��B
��B
�&B
5mB
�B
vB	�
B	��B	�B	�B	�B	��B	�B	�%B	�B	��B	�B	~7B	q�B	_�B	K
B	5�B	*JB	�B	�B	ZB��B�BߓB�{B�oB�3B�!B��B��B��B�gB�OB�1B�B��B��B�B��B��B��B��B��B��B��B��B��B��B��B��B�B�mBaB|OBy=Bw1BtBsBrBpBl�Bg�Bb�Ba�Bd�Ba�B_�BZ�BX~BX~BVrBTfBS`BPNBM<BL6BM=BL6BK0BK0BUmB~^B��B��B�B��B��B��B�cB�B��B��ByBBoBe�BScBL8BH!BQWBY�BVvBTjBSdBTkBh�Bc�Ba�B_�B\�BY�BR_BI)BFBFBDB?�B9�B?�BJ0BNIBMCBOOBK7BJ1BDB<�BCBNJBOPBUuBRbBX�BX�BY�BY�BX�BTpBQ]BORBSiBUvBSjBMFBL?BH(BNMBSjBQ_BV}BV~B]�Ba�Bg�Bw>Bw?Br!BmBn	Bv9BxFBv9Br"Bk�Bj�Bh�Bj�Bj�BmBk�BmBn
Bs)Bs)BwAByMBzSB~kB�xB�~BrB~kB}fB~kB�xB�xB�~B�~B��B��B��B��B��B��B��B��B�B�@B�eB�qB��B��B��B��B��B��B�B�B�?B�]B�]B�jBڍB�B��B��B�BަB�B��B��B�BݡB߭B��B��B��B��B�B�&B�!B�2B�]B�iB	 oB	�B	�B	�B	�B	�B	�B	�B	 B	B	$CB	%IB	%IB	([B	([B	'UB	&PB	*hB	3�B	6�B	6�B	5�B	6�B	7�B	;�B	<�B	<�B	=�B	>�B	>�B	>�B	<�B	:�B	=�B	>�B	@�B	A�B	B�B	D B	D B	D B	EB	GB	FB	FB	GB	HB	J$B	J$B	M7B	PIB	TaB	UgB	UgB	WsB	Z�B	\�B	^�B	`�B	_�B	b�B	d�B	k�B	l�B	m�B	n�B	qB	v)B	x5B	{GB	`B	�lB	�lB	�qB	�}B	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�'B	�-B	�:B	�@B	�RB	�^B	�dB	�pB	�vB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	� B	� B	�&B	�9B	�9B	�?B	�EB	�EB	�KB	�KB	�WB	�cG�O�B	�KB	�B	��B
�B
B
�B
+'B
2lB
8B
=�B
A�B
D�B
GB
M�B
W�B
ZZB
]�B
`�B
d�B
i�B
n�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.31 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9997(+/-0.0001), vertically averaged dS =-0.012(+/-0.004) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144232022020411442320220204114423  AO  ARCAADJP                                                                    20200619170923    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170923  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170923  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114423  IP                  G�O�G�O�G�O�                