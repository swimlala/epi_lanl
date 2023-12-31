CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:09:18Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200619170918  20220204114421  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               gA   AO  7662                            2C  D   APEX                            8312                            080318                          846 @���ua�X1   @�����@7X���F�c4z�G�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    gA   B   B   @���@���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�33B�33B�ffB���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[fD[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDy��D�)HD�W�D��\D�� D�!�D�T)D���D���D�
D�S�D��=Dǐ�D�{D�[�Dږ�D���D��D�P D�D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@���@�(�A{A>{A^{A~{A�
=A�
=A�
=A�
=A�
=A�
=A�
=A�
=B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B���B���B�(�B�\)B�B�B�B�B�B�B�B�B�B�B�B�B�B�Bˏ\B�B�B�B�B�B�B�B�B�B�B�B�B�C�HC�HC�HC�HC	�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC!�HC#�HC%�HC'�HC)�HC+�HC-�HC/�HC1�HC3�HC5�HC7�HC9�HC;�HC=�HC?�HCA�HCC�HCE�HCG�HCI�HCK�HCM�HCO�HCQ�HCS�HCU�HCW�HCY�HC[�HC]�HC_�HCa�HCc�HCe�HCg�HCi�HCk�HCm�HCo�HCq�HCs�HCu�HCw�HCy�HC{�HC}�HC�HC��C��C��C��C��C��C��qC��qC��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D xRD �RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RD	xRD	�RD
xRD
�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RD xRD �RD!xRD!�RD"xRD"�RD#xRD#�RD$xRD$�RD%xRD%�RD&xRD&�RD'xRD'�RD(xRD(�RD)xRD)�RD*xRD*�RD+xRD+�RD,xRD,�RD-xRD-�RD.xRD.�RD/xRD/�RD0xRD0�RD1xRD1�RD2xRD2�RD3xRD3�RD4xRD4�RD5xRD5�RD6xRD6�RD7xRD7�RD8xRD8�RD9xRD9�RD:xRD:�RD;xRD;�RD<xRD<�RD=xRD=�RD>xRD>�RD?xRD?�RD@xRD@�RDAxRDA�RDBxRDB�RDCxRDC�RDDxRDD�RDExRDE�RDFxRDF�RDGxRDG�RDHxRDH�RDIxRDI�RDJxRDJ�RDKxRDK�RDLxRDL�RDMxRDM�RDNxRDN�RDOxRDO�RDPxRDP�RDQxRDQ�RDRxRDR�RDSxRDS�RDTxRDT�RDUxRDU�RDVxRDV�RDWxRDW�RDXxRDX�RDYxRDY�RDZxRDZ��D[xRD[�RD\xRD\�RD]xRD]�RD^xRD^�RD_xRD_�RD`xRD`�RDaxRDa�RDbxRDb�RDcxRDc�RDdxRDd�RDexRDe�RDfxRDf�RDgxRDg�RDhxRDh�RDixRDi�RDjxRDj�RDkxRDk�RDlxRDl�RDmxRDm�RDnxRDn�RDoxRDo�RDpxRDp�RDqxRDq�RDrxRDr�RDsxRDs�RDtxRDt��Dy� D�%qD�S�D���D��)D��D�PRD���D���D�3D�P D��fDǍD��D�W�Dڒ�D��D��D�L)D�=D��\111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AٸRA�ƨA��A���A��#A���A���A��
A��
A���A���A��A��;A��/A��HA��mA��mA��mA��mA��mA��mA��yA��yA��yA��A��A��A��A��A��A��A��AمA�C�A֙�A�+A�hsA�7LA�E�A�?}A�"�A�t�AœuA�~�A��A��A���A�"�A�+A��PA�p�A�x�A���A��PA�(�A�=qA���A��;A��A��jA�|�A�dZA�&�A���A��\A��A��9A��A�t�A��#A�-A�ZA���A�\)A�;dA�JA��A���A���A��A��^A�JA���A���A�?}A�`BA�|�A��yA�9XA���A���A���A��PA��PA��A��HA��RA�{A��DA�\)A��A��!A��/A�n�A��TA�5?A�ƨA�bNA��hA��A�/A�l�A�ȴA��yA���A��-A��A��A��A�XA�p�A�bA�=qA���A�z�A�A�A�t�A�~�A�l�A�A~��Az��Aw&�At�HAr��Aq;dAn^5AlJAiK�Ag��AeS�Ac�mA`��A]A\JAZ�DAX5?AV=qAS��AR��AQt�AN�!AM"�AJ��AH��AH(�AH  AG��AF��AE`BAD��AC�AC
=ABffAA�AA�A@�yA@�!A?ƨA>�!A<��A;?}A:�/A:z�A9��A7O�A5;dA4bA/ƨA/K�A.��A.^5A-�#A,��A,VA*bNA)ƨA(ĜA'hsA$n�A"��A"bA v�A%A�A-A�
A;dA��A~�AA�A{A�A��A?}A��A�A�
AK�A
=A�9A��AA�A��AAC�A��A�-A^5A�AoA�A�A
A�A	A	�A�An�A��AXA33A;dA��A(�A%A�A z�A {@�~�@�1'@�o@���@��@�1@��;@�~�@�hs@�O�@�bN@��@�ƨ@�w@�@�$�@��#@�7@�G�@�j@��H@�$�@��^@�z�@��@�E�@�F@�Q�@�l�@��@ߍP@��@�@�j@㝲@�C�@�33@�o@�R@�\@�ff@�O�@�"�@�{@��/@���@���@�O�@��@�dZ@�I�@Ӆ@�dZ@�K�@�+@ҟ�@���@��/@�@�1'@ēu@�=q@��@�x�@���@���@�1'@�x�@�o@�@�x�@��@�ȴ@��P@�l�@�;d@��H@�S�@�K�@�@��P@�ȴ@���@�+@�~�@�n�@�v�@��7@�  @���@�
=@��\@���@�%@���@��9@�5?@���@�@�t�@��F@�`B@� �@�|�@���@��j@�Q�@��;@���@�l�@�"�@���@�ȴ@�E�@���@�O�@�G�@�7L@��@��/@��@��D@�z�@�9X@�ƨ@��@���@�-@��h@�`B@�X@�X@�?}@�7L@�&�@���@��@���@�(�@��@�|�@�S�@��H@��!@�V@�J@��#@��#@���@���@���@���@�@��^@��-@��h@�p�@�x�@�O�@�G�@�G�@�?}@���@�Q�@�b@��m@���@��;@�  @� �@�9X@�9X@�A�@�(�@� �@�b@�b@�b@�1@�  @���@�C�@��@�@���@��@��@��y@�v�@���@�%@�j@�1@�t�@�"�@��@�@�+@�;d@��@�ȴ@���@�M�@���@�G�@���@�Ĝ@��u@��u@��/@��/@���@��
@�
=@���@�^5@�J@��^@���@��@�hs@�X@�X@�O�@��@��u@�  @��w@��@��P@�l�@�"�@�o@�@��+@���@�@��-@�/@���@���@�%@��@��`@��`@���@��j@���@�z�@�Q�@�A�@�i�@{>�@vz@l��@d�@^&�@Uo @Q��@L��@G,�@?�k@7iD@4�Y@/�@)�@$�	@ �@�1@e,@1'@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  AٸRA�ƨA��A���A��#A���A���A��
A��
A���A���A��A��;A��/A��HA��mA��mA��mA��mA��mA��mA��yA��yA��yA��A��A��A��A��A��A��A��AمA�C�A֙�A�+A�hsA�7LA�E�A�?}A�"�A�t�AœuA�~�A��A��A���A�"�A�+A��PA�p�A�x�A���A��PA�(�A�=qA���A��;A��A��jA�|�A�dZA�&�A���A��\A��A��9A��A�t�A��#A�-A�ZA���A�\)A�;dA�JA��A���A���A��A��^A�JA���A���A�?}A�`BA�|�A��yA�9XA���A���A���A��PA��PA��A��HA��RA�{A��DA�\)A��A��!A��/A�n�A��TA�5?A�ƨA�bNA��hA��A�/A�l�A�ȴA��yA���A��-A��A��A��A�XA�p�A�bA�=qA���A�z�A�A�A�t�A�~�A�l�A�A~��Az��Aw&�At�HAr��Aq;dAn^5AlJAiK�Ag��AeS�Ac�mA`��A]A\JAZ�DAX5?AV=qAS��AR��AQt�AN�!AM"�AJ��AH��AH(�AH  AG��AF��AE`BAD��AC�AC
=ABffAA�AA�A@�yA@�!A?ƨA>�!A<��A;?}A:�/A:z�A9��A7O�A5;dA4bA/ƨA/K�A.��A.^5A-�#A,��A,VA*bNA)ƨA(ĜA'hsA$n�A"��A"bA v�A%A�A-A�
A;dA��A~�AA�A{A�A��A?}A��A�A�
AK�A
=A�9A��AA�A��AAC�A��A�-A^5A�AoA�A�A
A�A	A	�A�An�A��AXA33A;dA��A(�A%A�A z�A {@�~�@�1'@�o@���@��@�1@��;@�~�@�hs@�O�@�bN@��@�ƨ@�w@�@�$�@��#@�7@�G�@�j@��H@�$�@��^@�z�@��@�E�@�F@�Q�@�l�@��@ߍP@��@�@�j@㝲@�C�@�33@�o@�R@�\@�ff@�O�@�"�@�{@��/@���@���@�O�@��@�dZ@�I�@Ӆ@�dZ@�K�@�+@ҟ�@���@��/@�@�1'@ēu@�=q@��@�x�@���@���@�1'@�x�@�o@�@�x�@��@�ȴ@��P@�l�@�;d@��H@�S�@�K�@�@��P@�ȴ@���@�+@�~�@�n�@�v�@��7@�  @���@�
=@��\@���@�%@���@��9@�5?@���@�@�t�@��F@�`B@� �@�|�@���@��j@�Q�@��;@���@�l�@�"�@���@�ȴ@�E�@���@�O�@�G�@�7L@��@��/@��@��D@�z�@�9X@�ƨ@��@���@�-@��h@�`B@�X@�X@�?}@�7L@�&�@���@��@���@�(�@��@�|�@�S�@��H@��!@�V@�J@��#@��#@���@���@���@���@�@��^@��-@��h@�p�@�x�@�O�@�G�@�G�@�?}@���@�Q�@�b@��m@���@��;@�  @� �@�9X@�9X@�A�@�(�@� �@�b@�b@�b@�1@�  @���@�C�@��@�@���@��@��@��y@�v�@���@�%@�j@�1@�t�@�"�@��@�@�+@�;d@��@�ȴ@���@�M�@���@�G�@���@�Ĝ@��u@��u@��/@��/@���@��
@�
=@���@�^5@�J@��^@���@��@�hs@�X@�X@�O�@��@��u@�  @��w@��@��P@�l�@�"�@�o@�@��+@���@�@��-@�/@���@���@�%@��@��`@��`@���@��j@���@�z�@�Q�G�O�@�i�@{>�@vz@l��@d�@^&�@Uo @Q��@L��@G,�@?�k@7iD@4�Y@/�@)�@$�	@ �@�1@e,@1'@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
oB
oB
oB
oB
oB
oB
hB
oB
oB
oB
oB
oB
oB
oB
oB
oB
oB
oB
oB
oB
oB
oB
oB
oB
oB
oB
oB
oB
oB
oB
oB
oB
bB
DB
B	��B	��B
+B
VB
!�B
+B
-B
T�B
s�B
o�B
~�B
��B
��B
�LB
��B
�)B
�B
��B
��B%B'�B49BK�BS�Bl�Br�Bt�By�B�+B��B�3B�^B��B�B�ZB�B  B
=BPBbBuBuB�B�B%�B.B.B/B2-B5?B9XB>wBA�BI�BR�BR�BVBXBZB[#B`BB\)BQ�BO�BM�BK�BF�B@�B=qB6FB'�B�B%B�B�/B�dB��B|�Bp�Br�Bp�B\)BH�B%�BB
�B
�;B
B
��B
�JB
x�B
iyB
\)B
I�B
?}B
6FB
�B
B	�B	�)B	��B	�RB	��B	��B	�PB	~�B	r�B	bNB	M�B	>wB	6FB	%�B	�B	DB	%B	  B��B�yB�HB�B�
B�B��B��B��B��B��BɺBȴBŢBÖB��B��B�qB�RB�B��B��B��B��B��B�uB�hB�uB�{B�uB�uB�uB�oB�bB�VB�DB�1B�B~�By�Bw�Bz�Bz�Bz�Bz�B{�B{�B{�B{�B{�Bz�Bz�B{�B{�Bz�By�Bz�By�By�Bz�B{�B|�B|�Bz�By�Bx�By�Bu�Bv�Bv�Bv�Bw�Bv�Bu�Bu�Bu�Bq�Bq�Bp�Bq�Bt�Bs�Bu�Bu�Bt�Bu�Bu�By�By�By�Bz�B{�B{�Bz�Bz�B�B�B�%B�1B�=B�DB�JB�JB�JB�JB�JB�PB�PB�PB�DB�DB�JB�DB�\B�uB��B��B��B�3BɺB�B�HB�HB�HB�HB�NB�HB�HB�HB�BB�HB�NB�BB�B�B�B�
B��B��B��B��B��B��B��B��BB�B��B��B��B��B��B��B��B�XBŢB��BŢB��B��B�NB�sB�B�B��B��B��B�sB��B��B�
B�B�B�)B�)B�B�#B�B�)B�)B�B�HB�HB�B��BȴB��B��B��B��B��B�B�/B�BB�TB�fB�yB�B�B�B��B��B��B	  B	B	+B		7B	
=B	
=B	
=B	DB	VB	oB	�B	�B	�B	�B	�B	�B	�B	 �B	"�B	'�B	,B	1'B	33B	49B	5?B	6FB	;dB	=qB	@�B	@�B	A�B	C�B	D�B	D�B	F�B	H�B	K�B	M�B	N�B	P�B	R�B	W
B	YB	ZB	[#B	]/B	cTB	ffB	gmB	k�B	o�B	s�B	v�B	w�B	y�B	{�B	|�B	~�B	�B	�%B	�1B	�=B	�DB	�DB	�VB	�uB	�uB	�{B	�{B	�{B	�{B	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�'B	�9B	�RB	�jB	�jB	�^B	�RB	�^B	�jB	�wB	��B	��B	B	ÖB	ĜB	ĜB	ĜB	ŢB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�B	�
B	�B	�B	�#B	�)B	�/B	�/B	�5B	�5B	�;B	�;B	�;B	�;B	�;B	�B	��B	��B
dB
sB
!�B
)�B
3B
<6B
@iB
FYB
K^B
O(B
T�B
X�B
\�B
bhB
f�B
h�B
m�B
p�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B	�eB	�6B	�IB	��B
�B
B
!UB
#aB
KNB
jB
e�B
uHB
��B
�;B
��B
��B
�qB
��B
�!B
�4B
�jB3B*{BBBJ8Bb�Bh�Bj�BpB}gB�B�mB��B�B�NBڐB��B�5B qB�B�B	�B	�B�B�BB$FB$FB%MB(_B+qB/�B4�B7�B?�BI"BI#BL5BNABPNBQTBVsBRZBHBFBDBA�B<�B6�B3�B,{B&B�B�^B��B�lB��B� Bs3Bf�Bh�Bf�BRqB>�B0B
�VB
��B
ՍB
��B
�B
��B
o0B
_�B
R�B
@B
5�B
,�B
B	�rB	��B	ҒB	�CB	��B	�iB	��B	��B	ukB	i"B	X�B	DIB	4�B	,�B	\B	,B	�B��B�}B�GB��B��BΑB͋B̅B�B�zB�aB�UB�JB�=B�7B�%B�B�B�B��B��B��B�cB�LB�LB�EB�9B��B��B��B�B��B��B��B��B��B��B��B~�B{�Bu�BphBn\BqnBqnBqnBqnBrtBrtBruBruBruBqoBqoBruBruBqoBpiBqoBpiBpiBqoBruBs|Bs}BqpBpjBodBpjBlSBmYBmYBmYBn_BmZBlTBlTBlTBh;Bh<Bg6Bh<BkMBjHBlTBlUBkNBlUBlUBpmBpmBpmBqsBryBryBqsBqsBx�By�B|�B~�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�,B�JB�tB��B�HBϤB��B��B��B��B��B��B��B��B��B��B��B��BЫBϥB̒B͘B�hB�VB�\B�\B�\B�bB�\B�QB�B��B�^B�YB�(B�B�"B�;B��B��B�3B�RB�3B�XBʈB��B�B�B�>B�B�{B�QB�BɃBɃB͛BϨBЮBҺBҺBЮBѴBЮBҺBҺBЮB��B��BϩB�`B�GB�wBːB�rB�`BʊBЯB��B��B��B��B�
B�/B�5B�;B�SB�eB�wB��B��B��B��B	 �B	 �B	 �B	�B	�B	�B	!B	.B	:B	@B	FB	LB	LB	SB	_B	}B	"�B	'�B	)�B	*�B	+�B	,�B	1�B	3�B	7B	7B	8B	:"B	;'B	;'B	=3B	??B	BRB	D^B	EdB	GpB	I|B	M�B	O�B	P�B	Q�B	S�B	Y�B	\�B	]�B	bB	f&B	j>B	mQB	nWB	pcB	roB	suB	u�B	z�B	|�B	~�B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	�B	�B	�B	�B	�B	� B	�2B	�CB	�PB	�bB	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�%B	�+B	�=B	�PB	�VB	�\B	�gB	�nB	�zB	ˀB	͌B	͌B	ΒB	͌B	ΒB	ПB	ѥB	ҫB	ӱB	ӱB	ԷB	ԷB	սB	սB	սB	սG�O�B	ݟB	�[B	�3B
�B
�B
B
 B
)�B
2�B
6�B
<�B
A�B
E�B
K+B
O^B
S\B
X�B
]eB
_=B
dvB
gn111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.12 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.009(+/-0.004) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144212022020411442120220204114421  AO  ARCAADJP                                                                    20200619170918    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170918  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170918  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114421  IP                  G�O�G�O�G�O�                