CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-07-04T17:00:53Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200704170053  20220204114427  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @�&Zt�1   @�&�>�6@6P�`A�7�b���l�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @���@�  A��A   A@  A^ffA�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0ffB7��B?��BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C1�fC4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Ca�fCd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D-��D.� D/  D/�fD0fD0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJfDJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DXy�DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�3Dy��D�RD�h�D���D��=D�!HD�VfD��=D��HD�\D�W
D�� D��
D�=D�]Dڔ{D��3D��D�[�D��D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@�(�@�\*A{A>{A\z�A~{A�
=A�
=A�
=A�
=A�
=A�
=A�
=A�
=B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B�B�B�B�B�B�B�B�B�B�B���B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�\B�B�B�B�C�HC�HC�HC�HC	�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC!�HC#�HC%�HC'�HC)�HC+�HC-�HC/�HC1ǮC3�HC5�HC7�HC9�HC;�HC=�HC?�HCA�HCC�HCE�HCG�HCI�HCK�HCM�HCO�HCQ�HCS�HCU�HCW�HCY�HC[�HC]�HC_�HCaǮCc�HCe�HCg�HCi�HCk�HCm�HCo�HCq�HCs�HCu�HCw�HCy�HC{�HC}�HC�HC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��qC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C���C��C��C��C��C��C��C��C��C��qC��C��C��C��D xRD �RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RD	xRD	�RD
xRD
�RDxRD�RDxRD�RDxRD�RDxRD��DxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RD xRD �RD!xRD!�RD"xRD"�RD#xRD#�RD$xRD$�RD%xRD%�RD&xRD&�RD'xRD'�RD(xRD(�RD)xRD)�RD*xRD*�RD+xRD+�RD,xRD,�RD-xRD-��D.xRD.�RD/~�D/��D0xRD0�RD1xRD1�RD2xRD2�RD3xRD3�RD4xRD4�RD5xRD5�RD6xRD6�RD7xRD7�RD8xRD8�RD9xRD9�RD:xRD:�RD;xRD;�RD<xRD<�RD=xRD=�RD>xRD>�RD?xRD?�RD@xRD@�RDAxRDA�RDBxRDB�RDCxRDC�RDDxRDD�RDExRDE�RDFxRDF�RDGxRDG�RDHxRDH�RDIxRDI��DJxRDJ�RDKxRDK�RDLxRDL�RDMxRDM�RDNxRDN�RDOxRDO�RDPxRDP�RDQxRDQ�RDRxRDR�RDSxRDS�RDTxRDT�RDUxRDU�RDVxRDV�RDWxRDW�RDXq�DX�RDYxRDY�RDZxRDZ�RD[xRD[�RD\xRD\�RD]xRD]�RD^xRD^�RD_xRD_�RD`xRD`�RDaxRDa�RDbxRDb�RDcxRDc�RDdxRDd�RDexRDe�RDfxRDf�RDgxRDg�RDhxRDh�RDixRDi�RDjxRDj�RDkxRDk�RDlxRDl�RDmxRDm�RDnxRDn�RDoxRDo�RDpxRDp�RDqxRDq�RDrxRDr�RDsxRDs�RDtxRDt˅Dy��D�{D�eD��
D��fD�qD�R�D��fD��qD��D�S3D��)D��3D�fD�YHDڐ�D��\D��D�X D�D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A̩�A̩�Ạ�A̝�A̛�A̕�A�dZA˝�A�1'A�JA��HAʴ9A�VAɑhA�v�A�n�A�`BA�VA�I�A�;dA�+A���A�JA��mA��mA��yA��TA��`A�C�A�+A�M�A���A�(�A�  A�;dA�33A���A��^A���A�=qA��`A�A�\)A�`BA��DA���A�
=A��jA���A�O�A��!A���A�5?A��RA�ZA�33A�A���A��\A�M�A��;A�p�A��+A�ĜA��TA�?}A�ȴA�`BA��yA�E�A�
=A�n�A�JA���A���A�p�A�1'A�"�A��
A�x�A�?}A���A�z�A�1'A��A��PA�
=A�M�A���A�bA�ƨA�+A���A�ffA���A��A���A��`A�~�A��A�~�A�dZA�C�A�^5A�|�A�
=A��-A� �A��A���A�+A��A���A�33A���A�VA�r�A�VA�ffA�l�A�A�A��RA��wA�A��A�-A�+A���A�XA+A}�;A|9XAyXAvr�At��At�Ar�uAp(�AlffAi33Af�\Ad(�A`�A]ƨA[`BAZ-AWoAU�ATVASK�AQ��AP�uAO|�AL��AK�AHffAF��AD��AB�9A@�\A@ �A?A?`BA=�A:�A9�mA9�PA9XA7��A2�A3oA2��A1�#A0�HA/��A.�jA-;dA,9XA* �A(�A(9XA'\)A&�jA&�A$A�A#�A!�;AA�A�/A1A(�A��AbNAO�A��A�^A��AI�Al�A7LA~�A&�A�`A�\A{A?}A	�
A	��A	XAȴAVA��A��A�
A��A��A1A�A�A~�AXA n�A 1@�@��@�{@�"�@�n�@�^5@�n�@�-@�dZ@���@�n�@�`B@�ƨ@��@�$�@���@��@�D@�C�@��@�r�@�p�@�D@���@��@���@�"�@�%@܃@�"�@٩�@���@�bN@�1'@�|�@�E�@�V@�S�@Ь@��@��@�p�@�Q�@˥�@�ȴ@�5?@ɉ7@���@���@š�@���@�(�@���@��@�n�@�G�@��F@��!@���@�&�@��j@�A�@�  @��F@��!@��h@���@��m@�5?@��j@�bN@�bN@���@�$�@�hs@���@���@���@���@���@�Q�@�I�@�(�@��;@��@�"�@��R@�J@���@�&�@���@�ƨ@�o@�V@�G�@���@�9X@�b@�ƨ@�ƨ@��w@�l�@�~�@�V@�ȴ@�b@�?}@��@��@�(�@��F@�C�@�K�@��\@�V@�bN@��@��;@���@���@�O�@��@�Q�@��m@���@�"�@���@���@���@��u@���@�5?@�{@���@���@�ƨ@��@��#@�/@�G�@��7@���@��@��T@���@�5?@�M�@�V@�^5@�E�@��\@���@��R@���@��\@�-@��w@�C�@�K�@�\)@��w@�j@��j@�1'@�K�@�t�@��@���@�5?@�J@�@��;@�E�@��R@���@�v�@���@��@�@���@���@�M�@��@�5?@�M�@�$�@���@�G�@�7L@�/@�G�@�G�@���@� �@�b@�b@��
@��w@�t�@�33@��H@��+@��#@�X@�/@�p�@�E�@�=q@��#@�7L@��@���@��F@�|�@�33@�;d@�K�@��@��+@�n�@�ff@�n�@�{@��7@�G�@��@���@���@��D@��D@�A�@�1@��;@��
@��P@�dZ@�S�@���@��!@���@��+@�^5@�J@��T@��#@�@�X@�V@���@��u@��/@��j@��@���@��D@�Q�@�9X@� �@�1@��F@��P@�|�@�\)@�S�@�K�@�K�@�C@��'@x�[@o,�@c��@\-�@U[W@P�e@G�W@@�@:��@5Y�@/��@*��@$��@\)@J@�@8@ i@
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A̩�A̩�Ạ�A̝�A̛�A̕�A�dZA˝�A�1'A�JA��HAʴ9A�VAɑhA�v�A�n�A�`BA�VA�I�A�;dA�+A���A�JA��mA��mA��yA��TA��`A�C�A�+A�M�A���A�(�A�  A�;dA�33A���A��^A���A�=qA��`A�A�\)A�`BA��DA���A�
=A��jA���A�O�A��!A���A�5?A��RA�ZA�33A�A���A��\A�M�A��;A�p�A��+A�ĜA��TA�?}A�ȴA�`BA��yA�E�A�
=A�n�A�JA���A���A�p�A�1'A�"�A��
A�x�A�?}A���A�z�A�1'A��A��PA�
=A�M�A���A�bA�ƨA�+A���A�ffA���A��A���A��`A�~�A��A�~�A�dZA�C�A�^5A�|�A�
=A��-A� �A��A���A�+A��A���A�33A���A�VA�r�A�VA�ffA�l�A�A�A��RA��wA�A��A�-A�+A���A�XA+A}�;A|9XAyXAvr�At��At�Ar�uAp(�AlffAi33Af�\Ad(�A`�A]ƨA[`BAZ-AWoAU�ATVASK�AQ��AP�uAO|�AL��AK�AHffAF��AD��AB�9A@�\A@ �A?A?`BA=�A:�A9�mA9�PA9XA7��A2�A3oA2��A1�#A0�HA/��A.�jA-;dA,9XA* �A(�A(9XA'\)A&�jA&�A$A�A#�A!�;AA�A�/A1A(�A��AbNAO�A��A�^A��AI�Al�A7LA~�A&�A�`A�\A{A?}A	�
A	��A	XAȴAVA��A��A�
A��A��A1A�A�A~�AXA n�A 1@�@��@�{@�"�@�n�@�^5@�n�@�-@�dZ@���@�n�@�`B@�ƨ@��@�$�@���@��@�D@�C�@��@�r�@�p�@�D@���@��@���@�"�@�%@܃@�"�@٩�@���@�bN@�1'@�|�@�E�@�V@�S�@Ь@��@��@�p�@�Q�@˥�@�ȴ@�5?@ɉ7@���@���@š�@���@�(�@���@��@�n�@�G�@��F@��!@���@�&�@��j@�A�@�  @��F@��!@��h@���@��m@�5?@��j@�bN@�bN@���@�$�@�hs@���@���@���@���@���@�Q�@�I�@�(�@��;@��@�"�@��R@�J@���@�&�@���@�ƨ@�o@�V@�G�@���@�9X@�b@�ƨ@�ƨ@��w@�l�@�~�@�V@�ȴ@�b@�?}@��@��@�(�@��F@�C�@�K�@��\@�V@�bN@��@��;@���@���@�O�@��@�Q�@��m@���@�"�@���@���@���@��u@���@�5?@�{@���@���@�ƨ@��@��#@�/@�G�@��7@���@��@��T@���@�5?@�M�@�V@�^5@�E�@��\@���@��R@���@��\@�-@��w@�C�@�K�@�\)@��w@�j@��j@�1'@�K�@�t�@��@���@�5?@�J@�@��;@�E�@��R@���@�v�@���@��@�@���@���@�M�@��@�5?@�M�@�$�@���@�G�@�7L@�/@�G�@�G�@���@� �@�b@�b@��
@��w@�t�@�33@��H@��+@��#@�X@�/@�p�@�E�@�=q@��#@�7L@��@���@��F@�|�@�33@�;d@�K�@��@��+@�n�@�ff@�n�@�{@��7@�G�@��@���@���@��D@��D@�A�@�1@��;@��
@��P@�dZ@�S�@���@��!@���@��+@�^5@�J@��T@��#@�@�X@�V@���@��u@��/@��j@��@���@��D@�Q�@�9X@� �@�1@��F@��P@�|�@�\)@�S�@�K�G�O�@�C@��'@x�[@o,�@c��@\-�@U[W@P�e@G�W@@�@:��@5Y�@/��@*��@$��@\)@J@�@8@ i@
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
n�B
o�B
n�B
n�B
n�B
n�B
m�B
k�B
gmB
e`B
dZB
cTB
^5B
YB
XB
XB
XB
YB
YB
YB
YB
T�B
r�B
�dB
ÖB
�jB
��B
��B
�
B
�B2-B8RB$�B�BDB"�B/B7LBS�BM�B.B{BbB'�B<jB6FB1'BG�BjB�uB��Bq�BbNBv�B�+B�\B��B��B��B�B�wB��B�ZB�B��B��BBPB�B!�B$�B/B7LB<jB<jBC�BM�B[#Bl�Bx�B�+B�=B�VB�oB�\B{�B[#BR�BK�BC�BL�B=qB5?B'�B�BVBVBBB�B�)B��B�dB�B��B�hB�PB�+Be`B[#BR�BA�B/B#�B�B�B	7BB
��B
�`B
��B
��B
ÖB
�?B
�B
��B
�%B
w�B
\)B
O�B
D�B
9XB
!�B
\B	��B	��B	�B	�#B	��B	��B	�{B	�B	jB	ZB	H�B	?}B	.B	!�B	�B	�B	PB	B��B�B�`B��BĜB�XB�'B�!B�B�B��B��B�bB�JB�7B�B� Bn�B�%B�oB�\B�hB�oB�hB�JB�+B�PB�VB�oB��B�oB�bB�B~�Bu�B[#BR�BT�BbNB�B�1B�%B�B�B{�Bt�BgmBT�BL�BG�B7LB5?B5?B7LB5?B5?B6FB6FB9XB<jB>wBK�BN�BB�B>wB<jB;dB8RB33B.B.B5?BG�BG�BZBbNBffBiyBk�BjB`BB_;B_;BaHB`BB_;BbNBcTBdZBe`BgmBgmBjBiyBl�Bm�Bm�Bn�Bn�Bm�Bm�Bm�Bn�Bn�Bm�Bo�Bn�Bo�Bs�Bu�Bu�Bu�Bw�By�By�B|�B~�B�B�B�B�B�B�B�B�%B�%B�DB�VB�\B�hB�oB�uB�{B�{B�{B��B��B��B��B��B��B��B��B��B��B�'B�RB�wB�wB��BBÖBÖBĜBĜBŢBŢBƨBȴB��B��B��B��B��B��B��B��B��B��B��B�
B�B�B�/B�;B�TB�B��B	B	B		7B	DB	DB	DB	PB	VB	\B	bB	bB	\B	hB	oB	uB	{B	�B	�B	�B	 �B	#�B	)�B	1'B	:^B	=qB	?}B	?}B	=qB	9XB	49B	8RB	?}B	C�B	H�B	K�B	N�B	O�B	VB	\)B	_;B	`BB	aHB	bNB	jB	o�B	r�B	w�B	x�B	x�B	{�B	{�B	|�B	~�B	�B	�+B	�JB	�JB	�VB	�oB	�{B	�oB	�hB	�bB	�bB	�=B	�%B	�=B	�JB	�JB	�VB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�!B	�'B	�B	�B	�B	�'B	�RB	�XB	�dB	�^B	�jB	�dB	�jB	�qB	�wB	��B	ƨB	ƨB	ƨB	ǮB	ǮB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�)B	�5B	�5B	�;B	�;B	�;B	�;B	�;B	�BB	�BB	�HB	�HB	�HB	�`B	�mB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B
 iB
�B
=B
&�B
/iB
7�B
>]B
H�B
MB
R�B
X�B
]�B
b�B
g8B
lqB
qB
u%B
yXB
|�B
� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
^>B
_DB
^>B
^>B
^>B
^>B
]7B
[,B
WB
UB
TB
R�B
M�B
H�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
D�B
bWB
�B
�3B
�	B
�|B
B
ƦB
�IB!�B'�BpBB
��BeB�B&�BC�B=bB�BB
��B�B+�B%�B �B7?BZB��B�Ba6BQ�BfUBv�B~�B�B�4B�^B��B��B�gB��B�
B�:B�qB��B��B&BDBVB�B&�B+�B+�B3B=HBJ�B[�BhEBv�By�B}�B��B~�BkWBJ�BBhB;>B3B<DB,�B$�BmB	B��B��B��B�B�5B˱B�cB��B��B�B��B|�Bv�BT�BJ�BB�B1(B�BzBbB8B
��B
�B
�fB
�B
ĭB
��B
�GB
��B
��B
�]B
u�B
g�B
K�B
?�B
4`B
)B
�B	�(B	�B	�B	�VB	��B	�ZB	��B	�YB	t�B	ZcB	JB	8�B	/gB	B	�B	�B	rB�BB�B��B�B�XB��B��B�WB�(B�"B�B�
B��B��B�hB|QBy>Bu'Bp	B^�Bv.B�wBdB�pB�wB�qB|TBw5B}ZB~`B�yB��B�yB�mBt&BoBe�BK6BCBEBR`BqBx?Bv3BrBrBk�Bd�BW�BEB<�B7�B'gB%ZB%ZB'gB%[B%[B&bB&bB)tB,�B.�B;�B>�B2�B.�B,�B+�B(oB#QB3B3B%]B7�B7�BJ7BRgBV~BY�B[�BZ�BP\BOUBOUBQbBP]BOVBRiBSoBTuBU{BW�BW�BZ�BY�B\�B]�B]�B^�B^�B]�B]�B]�B^�B^�B]�B_�B^�B_�Bc�Be�Be�Be�Bg�Bi�Bi�Bm
BoBq"Br(Bs.Bt5Bt5Bu;Bu;BvABvAB{_B~qBwB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�@B�jB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�%B�2B�DB�PB�hBۘB��B�"B�/B�FB�SB�SB�SB�_B�eB�kB	 qB	 qB�kB	wB	~B	�B	�B	�B	�B	�B	�B	�B	B	!2B	*hB	-{B	/�B	/�B	-{B	)bB	$DB	(]B	/�B	3�B	8�B	;�B	>�B	?�B	FB	L/B	OAB	PHB	QNB	RTB	Z�B	_�B	b�B	g�B	h�B	h�B	k�B	k�B	l�B	n�B	qB	w,B	|KB	|KB	~WB	�oB	�{B	�oB	�hB	�cB	�cB	z?B	v'B	z?B	|LB	|LB	~WB	�pB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�B	�B	�B	�B	�%B	�B	�B	�B	�%B	�OB	�UB	�`B	�[B	�fB	�aB	�gB	�mB	�sB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�"B	�.B	�.B	�4B	�4B	�4B	�4B	�4B	�;B	�;B	�AB	�AB	�AB	�XB	�eB	�kB	�kB	�qB	�wB	�wB	�}B	܃B	݉B	݉B	ސB	ސB	ސB	ߕG�O�B	��B	�_B
�B
1B
uB
\B
'�B
.OB
8�B
=B
BzB
H�B
M�B
R�B
W(B
\`B
`�B
eB
iGB
ltB
o�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.12 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9996(+/-0.0001), vertically averaged dS =-0.016(+/-0.004) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144272022020411442720220204114427  AO  ARCAADJP                                                                    20200704170053    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200704170053  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200704170053  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114427  IP                  G�O�G�O�G�O�                