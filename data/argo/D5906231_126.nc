CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-01-27T10:01:57Z creation      
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
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �0   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �@   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �D   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �T   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �X   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �\   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �`Argo profile    3.1 1.2 19500101000000  20230127100157  20230308100451  5906231 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               ~A   AO  8375                            2C  D   APEX                            8809                            080319                          846 @�O�(�p1   @�RβI��D>z�G�@D�bM��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    ~A   B   B   @���@���A   A   A>ffA`  A���A�  A�  A���A���A�  A�  A�33B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  BhffBp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�  B���C   C  C  C�C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C?�fCB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C��C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��C��C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C��3C�  C�  C��3C�  C�  C��3C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� DfD� DfD� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D3��D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=fD=� D>fD>� D?  D?� D@  D@� DAfDA� DB  DB� DC  DC� DD  DD� DD��DE� DF  DF� DG  DG� DH  DHy�DI  DI� DJ  DJy�DK  DK� DL  DLy�DM  DM�fDNfDN� DO  DO�fDPfDP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY�fDZ  DZ� D[  D[� D\  D\� D\��D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl�fDmfDm�fDn  Dn� Do  Do� Dp  Dp� Dq  Dqy�Dr  Dr� Ds  Ds� Dt  Dt� Dt� Dy�
D��D�XRD�O\D��3D��D�_
D��D��D�qD�W\D���D��3D� D�>fDږfD�ǮD���D�S3D� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�@���@�(�A{A<z�A^{A�A�
=A�
=A��
A��
A�
=A�
=A�=pA�
=B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B�B�B�B�B�B�B�B�B�B�B�B�B���B�B�B�B�B�B�B�B�B�B�B�B�B�B�\B�\B�B�B��\B�C�HC�HC��C�HC	�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC!�HC#�HC%�HC'�HC)�HC+�HC-�HC/�HC1�HC3�HC5�HC7�HC9�HC;�HC=�HC?ǮCA�HCC�HCE�HCG�HCI�HCK�HCM�HCO�HCQ�HCS�HCU�HCW�HCY�HC[�HC]�HC_�HCa�HCc�HCe�HCg�HCi�HCk�HCm�HCo�HCq�HCs�HCu�HCw�HCy�HC{�HC}�HC��C��C��C��C��C��qC��C��C��C��C��C���C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��qC��C��C��C��qC��qC��C��C��C��C���C���C��C��C��C��C���C��C��C���C��C��C���C��C��C��qC��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C���C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��qC��C��C��C��C��C��C��C��C��C��qC��C��qC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D xRD �RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD��DxRD��DxRD�RD	xRD	�RD
xRD
�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RD xRD �RD!xRD!�RD"xRD"�RD#xRD#�RD$xRD$�RD%xRD%�RD&xRD&�RD'xRD'�RD(xRD(�RD)xRD)�RD*xRD*�RD+xRD+�RD,xRD,�RD-xRD-�RD.xRD.�RD/xRD/�RD0xRD0�RD1xRD1�RD2xRD2�RD3xRD3��D4xRD4�RD5xRD5�RD6xRD6�RD7xRD7�RD8xRD8�RD9xRD9�RD:xRD:�RD;xRD;�RD<xRD<��D=xRD=��D>xRD>�RD?xRD?�RD@xRD@��DAxRDA�RDBxRDB�RDCxRDC�RDDxRDD��DExRDE�RDFxRDF�RDGxRDG�RDHq�DH�RDIxRDI�RDJq�DJ�RDKxRDK�RDLq�DL�RDM~�DM��DNxRDN�RDO~�DO��DPxRDP�RDQxRDQ�RDRxRDR�RDSxRDS�RDTxRDT�RDUxRDU�RDVxRDV�RDWxRDW�RDXxRDX�RDY~�DY�RDZxRDZ�RD[xRD[�RD\xRD\��D]xRD]�RD^xRD^�RD_xRD_�RD`xRD`�RDaxRDa�RDbxRDb�RDcxRDc�RDdxRDd�RDexRDe�RDfxRDf�RDgxRDg�RDhxRDh�RDixRDi�RDjxRDj�RDkxRDk�RDl~�Dl��Dm~�Dm�RDnxRDn�RDoxRDo�RDpxRDp�RDqq�Dq�RDrxRDr�RDsxRDs�RDtxRDt�RDy�\D���D�T{D�K�D��\D��D�[3D��=D��HD�	�D�S�D���D��\D�)D�:�Dڒ�D���D��D�O\D�)11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A��/A���A�ƨA�ƨA���A��wA��FA���A�\)A���A�
=A�+A��A�t�A�33A��A�bA�XA���A�1A�l�A�|�A�E�A�~�A���A��/A���A�A��\A��A���A��hA���A�M�A� �A�A��A�/A~�\A}�mA}7LA{��A{�#A{�-A{dZA{Az��AzQ�Ay�Az �AzI�Azz�Az��Az�9Az��Az�uAz=qAy��Ay�Ay�-AyƨAy�;Ay�Ay�mAy�;Ay��Ay�^Ay��Ay��Ay�Ayl�Ay`BAyXAyG�AyC�AyC�AyC�Ay33Ay&�AyoAx��Ax�`Ax��Ax��Ax�Ax��Ax�+Axv�AxbNAxE�Ax-Ax�AxAw�#Aw��AwXAw33Av��Av��Avv�Au�Au|�At��As�#Ar�HAr=qAq��Aq�Ap��AoVAn�Am`BAk��Ak/AjQ�Ai��AhȴAhbNAg�hAf �Ae�FAeG�Ad�Ac�-Ac33AcoAbQ�A`ĜA`�A`A_x�A^�A^9XA^{A]�;A]S�A\~�A[��AZ�AZz�AZI�AZAY��AX  AV�/AV�\AU�^AT��ATE�AT1AS�AS�-AS/AR�RAR-AQVAPv�AO��AN�uAN�DANz�ANn�AM�
AMALI�AKhsAJr�AI��AIXAI�AH�AH�/AHv�AH=qAH$�AG�AG�AGt�AG\)AG?}AG%AF��AF�DAF~�AFn�AFffAF$�AE��AE`BAD�AD�!ADv�ADI�AD1AC|�ACdZAB~�AA�wA@��A?�A=�A<�A<~�A;�wA;?}A:�yA:�A9��A8�`A8bNA7�A6��A6��A6ȴA7XA7��A7�FA7�mA8bNA8�`A9dZA9;dA9�A9�A9oA8��A8��A8ffA6��A6��A6VA5�FA5/A4�A4�+A49XA4��A4��A5+A4�uA3�A3�A3�PA3t�A3+A2�jA21'A1A1%A0��A/��A/�A/VA.^5A.I�A.^5A.ZA. �A.{A-��A-XA,��A,��A,-A+oA)�wA(ZA'�-A'\)A&�A&�yA&ĜA&�A&9XA%��A%oA$ȴA$�!A$z�A$VA#K�A!��A!;dA ��A�7A%A�A��Av�AZAJA�AȴAJAG�A��AI�A�wA��A��AK�A��A�\An�A-AA��A�yA�!An�A5?AA��AVAA�AȴA�A�Az�Av�A�AG�AȴA��AZA�#A��A&�A��AȴAbNA  A�#A�^At�AĜA�A
��A
^5A
�A	��A	?}A��AjA �AJA�An�A��A�AdZA+A��AZA(�A�A�A
=AȴA�!A�uAn�AbNA�A
=A�`A�A�^A�A �yA ��A (�A b@��
@���@���@�x�@�?}@���@�z�@�j@�bN@�1@��@�`B@�(�@��H@�=q@���@��@�1'@�|�@�l�@�t�@�C�@�\@�h@��@�33@��H@���@�x�@�@�~�@��@���@旍@�x�@�Q�@�l�@�J@��@߾w@ޗ�@���@ݑh@�V@�j@�ƨ@ڸR@ّh@���@�b@�l�@��#@Ԭ@��m@�\)@ҟ�@љ�@�Ĝ@�Z@ϥ�@�C�@���@�@�7L@̓u@�Q�@���@�S�@ʧ�@ə�@���@�I�@��
@�;d@�ȴ@�n�@�E�@��@�X@��`@�(�@��@�=q@�?}@��P@��@�{@�G�@���@��!@���@��@��;@�"�@�J@��^@�`B@��@���@�9X@�"�@�@�O�@��/@��u@��;@�ƨ@�C�@�v�@�5?@��@�O�@�j@�1'@�1'@��@��6@�N<@���@�d�@}%F@v�@o'�@e!�@[ƨ@V��@N�y@Ea�@Ec�@?��@>@�@:C�@5@@4�@2��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   A��A��/A���A�ƨA�ƨA���A��wA��FA���A�\)A���A�
=A�+A��A�t�A�33A��A�bA�XA���A�1A�l�A�|�A�E�A�~�A���A��/A���A�A��\A��A���A��hA���A�M�A� �A�A��A�/A~�\A}�mA}7LA{��A{�#A{�-A{dZA{Az��AzQ�Ay�Az �AzI�Azz�Az��Az�9Az��Az�uAz=qAy��Ay�Ay�-AyƨAy�;Ay�Ay�mAy�;Ay��Ay�^Ay��Ay��Ay�Ayl�Ay`BAyXAyG�AyC�AyC�AyC�Ay33Ay&�AyoAx��Ax�`Ax��Ax��Ax�Ax��Ax�+Axv�AxbNAxE�Ax-Ax�AxAw�#Aw��AwXAw33Av��Av��Avv�Au�Au|�At��As�#Ar�HAr=qAq��Aq�Ap��AoVAn�Am`BAk��Ak/AjQ�Ai��AhȴAhbNAg�hAf �Ae�FAeG�Ad�Ac�-Ac33AcoAbQ�A`ĜA`�A`A_x�A^�A^9XA^{A]�;A]S�A\~�A[��AZ�AZz�AZI�AZAY��AX  AV�/AV�\AU�^AT��ATE�AT1AS�AS�-AS/AR�RAR-AQVAPv�AO��AN�uAN�DANz�ANn�AM�
AMALI�AKhsAJr�AI��AIXAI�AH�AH�/AHv�AH=qAH$�AG�AG�AGt�AG\)AG?}AG%AF��AF�DAF~�AFn�AFffAF$�AE��AE`BAD�AD�!ADv�ADI�AD1AC|�ACdZAB~�AA�wA@��A?�A=�A<�A<~�A;�wA;?}A:�yA:�A9��A8�`A8bNA7�A6��A6��A6ȴA7XA7��A7�FA7�mA8bNA8�`A9dZA9;dA9�A9�A9oA8��A8��A8ffA6��A6��A6VA5�FA5/A4�A4�+A49XA4��A4��A5+A4�uA3�A3�A3�PA3t�A3+A2�jA21'A1A1%A0��A/��A/�A/VA.^5A.I�A.^5A.ZA. �A.{A-��A-XA,��A,��A,-A+oA)�wA(ZA'�-A'\)A&�A&�yA&ĜA&�A&9XA%��A%oA$ȴA$�!A$z�A$VA#K�A!��A!;dA ��A�7A%A�A��Av�AZAJA�AȴAJAG�A��AI�A�wA��A��AK�A��A�\An�A-AA��A�yA�!An�A5?AA��AVAA�AȴA�A�Az�Av�A�AG�AȴA��AZA�#A��A&�A��AȴAbNA  A�#A�^At�AĜA�A
��A
^5A
�A	��A	?}A��AjA �AJA�An�A��A�AdZA+A��AZA(�A�A�A
=AȴA�!A�uAn�AbNA�A
=A�`A�A�^A�A �yA ��A (�A b@��
@���@���@�x�@�?}@���@�z�@�j@�bN@�1@��@�`B@�(�@��H@�=q@���@��@�1'@�|�@�l�@�t�@�C�@�\@�h@��@�33@��H@���@�x�@�@�~�@��@���@旍@�x�@�Q�@�l�@�J@��@߾w@ޗ�@���@ݑh@�V@�j@�ƨ@ڸR@ّh@���@�b@�l�@��#@Ԭ@��m@�\)@ҟ�@љ�@�Ĝ@�Z@ϥ�@�C�@���@�@�7L@̓u@�Q�@���@�S�@ʧ�@ə�@���@�I�@��
@�;d@�ȴ@�n�@�E�@��@�X@��`@�(�@��@�=q@�?}@��P@��@�{@�G�@���@��!@���@��@��;@�"�@�J@��^@�`B@��@���@�9X@�"�@�@�O�@��/@��u@��;@�ƨ@�C�@�v�@�5?@��@�O�@�j@�1'G�O�@��@��6@�N<@���@�d�@}%F@v�@o'�@e!�@[ƨ@V��@N�y@Ea�@Ec�@?��@>@�@:C�@5@@4�@2��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B�B�B�yB�B��B��BB��B%BbB	7BBB\BVBPB
=B+BBBBB��B��B��B��BPBbB\BPB+B��B�B�B�B�yB�sB�sB�yB�B��BBbB�B�B�B&�B,B-B.B-B,B-B0!B2-B49B6FB7LB8RB7LB7LB7LB6FB6FB5?B5?B5?B5?B49B5?B49B33B33B2-B1'B0!B/B/B.B,B,B+B)�B'�B&�B%�B$�B �B�B�B�B�BuBPB+B��B�B�sB�HB�#B��B��B�qB�XB�B��B�uB�DB�B}�By�Bt�BgmBdZB_;BYBQ�BK�BI�BE�B7LB-B-B'�B�B�B�B�B{BPBB  B��B��B��B�B�mB�)B�B��B��BƨBĜBÖB��B�qB�RB�-B��B��B��B�hB�hB�\B�\B�=Be`Bz�Bq�BhsBbNB^5B[#BZBYBVBS�BR�BP�BM�BL�BK�BJ�BH�BF�BD�BE�BD�BC�BC�BA�B?}B=qB:^B9XB8RB6FB0!B.B'�B�BhBB�B�`B�BB�
B��B��BȴB�}B�XB�?B�B��B��B�B�LB�qB��BŢB��B�B�sB�B�B�B�B�B�B�sB�)B�B��B��BȴBĜBÖB��BɺB��B�B��BȴBĜBĜBŢBĜB�}B�dB�FB�B��B��B��B��B�{B��B��B��B��B��B��B�\B�7B�%B� Bu�Be`BW
BL�BJ�BK�BP�BQ�BO�BL�BI�BE�BD�BC�BC�BF�B>wB1'B(�B�B�BbB\B\B\BVBPB+BB
��B
�B
�B
�B
�sB
�B
�B
�B
�B
�B
�B
�sB
�fB
�`B
�BB
�)B
�#B
�B
�
B
��B
��B
ǮB
�}B
�jB
�jB
�dB
�dB
�XB
�?B
�3B
�'B
�B
�B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�=B
�%B
� B
{�B
x�B
t�B
s�B
o�B
m�B
m�B
w�B
w�B
t�B
r�B
p�B
o�B
m�B
iyB
hsB
ffB
dZB
e`B
dZB
ffB
ffB
e`B
cTB
`BB
ZB
ZB
VB
O�B
M�B
H�B
F�B
D�B
C�B
B�B
@�B
<jB
;dB
?}B
?}B
?}B
A�B
C�B
G�B
I�B
F�B
B�B
<jB
9XB
7LB
2-B
0!B
,B
,B
.B
2-B
/B
-B
'�B
&�B
&�B
%�B
$�B
!�B
�B
�B
oB
VB
JB
1B
%B
B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�sB	�`B	�NB	�BB	�5B	�/B	�B	�B	�
B	�B	��B	��B	��B	��B	��B	��B	��B	ɺB	ɺB	ȴB	ǮB	ƨB	ĜB	ÖB	B	��B	��B	��B	�}B	�qB	�jB	�^B	�RB	�LB	�3B	�-B	�!B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�CB	�7B	��B	�oB	�-B	�B	�bB	�B
HB
'RB
9	B
F%B
ZQB
sB
x�B
�[B
�6B
�%B
�'B
�/11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   B��B��B��B��B��B��B��B��B�B�B�B�B��B��BB��B1BnB	CBB+BhBbB\B
IB7BB%B+B+B��B��B��B�B]BoBiB]B8B��B��B�B�B�B�B�B�B�B��B&BoB�B�B�B&�B,B-B.!B-B,B-B0.B2:B4FB6SB7YB8_B7YB7YB7YB6SB6SB5LB5LB5LB5LB4FB5LB4FB3@B3@B2:B14B0.B/(B/(B.!B,B,B+B*	B'�B&�B%�B$�B �B�B�B�B�B�B]B8B�B��B�B�UB�0B��B��B�~B�eB�B��B��B�QB�,B~By�Bt�BgzBdgB_HBY$BQ�BK�BI�BE�B7YB-B-B'�B�B�B�B�B�B]B,B B��B��B��B��B�zB�6B�$B��B��BƵBĩBãB��B�~B�_B�:B�B��B��B�uB�uB�iB�iB�JBemBz�Bq�Bh�Bb[B^BB[0BZ*BY$BVBTBR�BP�BM�BL�BK�BJ�BH�BF�BD�BE�BD�BC�BC�BA�B?�B=~B:kB9eB8_B6SB0.B.!B'�B�BuB,B�B�nB�PB�B��B��B��B��B�fB�MB�B��B��B�B�ZB�B��BŰB��B�%B�B�B�B�B�B�B�B�B�7B�B�B��B��BĪBäB��B��B�B�B��B��BĪBĪBŰBĪB��B�rB�TB�"B�
B��B��B��B��B��B��B��B��B��B��B�jB�EB�3B�Bu�BenBWBL�BJ�BK�BP�BQ�BO�BL�BI�BE�BD�BC�BC�BF�B>�B15B)B�B�BpBjBjBjBdB^B9B B
��B
��B
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
�tB
�nB
�PB
�7B
�1B
�%B
�B
� B
��B
ǼB
��B
�xB
�xB
�rB
�rB
�fB
�MB
�AB
�5B
�)B
�"B
�B
�B
��B
��B
��B
��B
��B
��B
��B
��B
�KB
�3B
�B
{�B
x�B
t�B
s�B
o�B
m�B
m�B
w�B
w�B
t�B
r�B
p�B
o�B
m�B
i�B
h�B
ftB
dhB
enB
dhB
ftB
ftB
enB
cbB
`PB
Z+B
Z+B
VB
O�B
M�B
H�B
F�B
D�B
C�B
B�B
@�B
<xB
;rB
?�B
?�B
?�B
A�B
C�B
G�B
I�B
F�B
B�B
<yB
9gB
7[B
2<B
00B
,B
,B
.#B
2<B
/*B
-B
'�B
&�B
&�B
%�B
$�B
!�B
�B
�B
~B
eB
YB
@B
4B
!B	�	B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�oB	�]B	�QB	�DB	�>B	�,B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	ǽB	ƷB	īB	åB	B	��B	��B	��B	��B	��B	�yB	�mB	�aB	�[B	�BB	�<B	�0B	�#B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��G�O�B	�RB	�FB	��B	�B	�=B	�B	�rB	��B
XB
'bB
9B
F5B
ZaB
s(B
x�B
�jB
�EB
�4B
�6B
�>11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.12 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             202303081004512023030810045120230308100451  AO  ARCAADJP                                                                    20230127100157    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20230127100157  QCP$                G�O�G�O�G�O�1F83E           AO  ARGQQCPL                                                                    20230127100157  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20230308100451  IP                  G�O�G�O�G�O�                