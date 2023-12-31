CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2017-06-09T17:03:06Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         (Matthew Alkire, University of Washington      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7,   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  74   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7t   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     88   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8X   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8x   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8|   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
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
resolution        :�o     �  qt   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �\   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �\   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �P   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �$   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �(   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �,Argo profile    3.1 1.2 19500101000000  20170609170306  20190604094028  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051                            2C  D   APEX                            5368                            041511                          846 @�㠝�N1   @��>�X@3�-V�d�����1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   BffB  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C3�fC6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtffDy��D��qD�E�D��\D��3D��D�@�D��=D��
D�qD�C�D��3D�ǮD�\D�A�Dڊ=D�vD��D�FfD�w�D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�@�(�@�(�A{A>{A^{A~{A�
=A�
=A�
=A�
=A�
=A�
=A�
=A�
=B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C�HC�HC�HC�HC	�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC!�HC#�HC%�HC'�HC)�HC+�HC-�HC/�HC1�HC3ǮC5�HC7�HC9�HC;�HC=�HC?�HCA�HCC�HCE�HCG�HCI�HCK�HCM�HCO�HCQ�HCS�HCU�HCW�HCY�HC[�HC]�HC_�HCa�HCc�HCe�HCg�HCi�HCk�HCm�HCo�HCq�HCs�HCu�HCw�HCy�HC{�HC}�HC�HC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D xRD �RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RD	xRD	�RD
xRD
�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RD xRD �RD!xRD!�RD"xRD"�RD#xRD#�RD$xRD$�RD%xRD%�RD&xRD&�RD'xRD'�RD(xRD(�RD)xRD)�RD*xRD*�RD+xRD+�RD,xRD,�RD-xRD-�RD.xRD.�RD/xRD/�RD0xRD0�RD1xRD1�RD2xRD2�RD3xRD3�RD4xRD4�RD5xRD5�RD6xRD6�RD7xRD7�RD8xRD8�RD9xRD9�RD:xRD:�RD;xRD;�RD<xRD<�RD=xRD=�RD>xRD>�RD?xRD?�RD@xRD@�RDAxRDA�RDBxRDB�RDCxRDC�RDDxRDD�RDExRDE�RDFxRDF�RDGxRDG�RDHxRDH�RDIxRDI�RDJxRDJ�RDKxRDK�RDLxRDL�RDMxRDM�RDNxRDN�RDOxRDO�RDPxRDP�RDQxRDQ�RDRxRDR�RDSxRDS�RDTxRDT�RDUxRDU�RDVxRDV�RDWxRDW�RDXxRDX�RDYxRDY�RDZxRDZ�RD[xRD[�RD\xRD\�RD]xRD]�RD^xRD^�RD_xRD_�RD`xRD`�RDaxRDa�RDbxRDb�RDcxRDc�RDdxRDd�RDexRDe�RDfxRDf�RDgxRDg�RDhxRDh�RDixRDi�RDjxRDj�RDkxRDk�RDlxRDl�RDmxRDm�RDnxRDn�RDoxRDo�RDpxRDp�RDqxRDq�RDrxRDr�RDsxRDs�RDt^�Dy�D�ٚD�A�D���D��\D��D�<�D��fD��3D��D�?�D��\D���D��D�=�DچfD�r=D��D�B�D�s�D��H11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Aִ9A֮Aְ!Aְ!AֶFAּjAּjAָRA֙�A�z�A�bAՏ\A�O�A��A���A��;A���AԺ^Aԥ�AԁA��A�t�A�Aџ�AθRA��#A���A��AƁA�VA���A���A�ƨA�bA�?}A�n�A�n�A���A�33A���A�XA��A��A�-A�=qA�A�XA���A�%A�ƨA��mA��9A��A�^5A�z�A��DA�JA�&�A���A�A��wA�z�A��!A���A��A���A��9A���A�/A��
A�O�A�XA�
=A���A�|�A��\A�v�A���A�+A���A��;A�z�A���A�-A�JA���A���A���A��A�1'A��A�z�A��hA��A���A��TA���A�z�A�dZA�/A�O�AG�A|AyhsAw��Av�At�Ar5?Ap1'Ak��Ai�AgC�AfA�Af �Adr�A^z�A[�A[�AXĜAW��AV-AUdZAS�TAQ�-AQdZAP�ALJAIt�AH�AH��AG�FAF�jAF1'AE�7AD��ABn�AA�wAAoA@��A@�jA@jA?dZA=�
A=&�A;��A;S�A:�!A:E�A9��A9p�A9�A8ĜA8A�A7�TA6��A3A2ZA1�A0�jA0-A/hsA.��A-�A,�+A+?}A*ZA(�A'��A';dA&�\A&�A%�A$JA#��A#O�A"��A��A{A��A;dA�`A5?A1A�jA�#AG�A��A��A��AZA+A1'AbNA��At�A\)A/A�A��A�hAdZA�A
=A�A�+A�-A��Ar�A9XA��A
 �A��A�^A��AA�hAK�A�uA�A�A �HA J@���@�1@��!@�x�@��/@�r�@�S�@��@�^@�(�@���@�+@�(�@�S�@�?}@�ƨ@���@�$�@�1'@�O�@��@�n�@�hs@��
@ڧ�@��@�X@؛�@��;@թ�@�A�@�
=@�J@���@�
=@Η�@�%@��m@˕�@�t�@�+@���@�ff@�V@�5?@�@�@��@��#@���@ƸR@�%@ċD@�9X@�|�@���@�G�@���@��9@���@�I�@��w@���@�j@�"�@��\@��+@��\@��+@��+@�~�@�~�@�~�@�~�@�V@��@�X@��@��`@�j@�9X@��@��
@�dZ@��\@�{@���@�x�@�Ĝ@�(�@��P@��\@�ff@�@�x�@�X@�/@��`@��D@�bN@�Q�@�Z@���@���@��@��w@�t�@��@��7@�G�@��u@�  @���@�+@��!@�n�@��@�hs@�  @��y@�ff@��@���@���@���@��7@�?}@�?}@��@���@��D@��@��F@�C�@�o@��!@�M�@�{@���@��#@��#@��T@��@��@�v�@��!@�v�@��@���@�&�@�%@�bN@�(�@��w@�S�@���@�$�@�@�X@��`@�9X@�ƨ@���@�S�@�"�@�ȴ@�n�@�M�@�=q@�J@��T@���@���@��7@�x�@��@��@�Ĝ@���@��D@�j@�(�@�b@��m@�S�@��R@��+@�^5@�E�@�E�@�-@�J@��@��^@��-@��^@��^@��-@��^@��^@��@��u@���@���@�%@���@��`@���@�bN@�Z@� �@���@��@�l�@�+@�ȴ@�ȴ@��!@�n�@�@�?}@��`@��/@���@�A�@��F@���@�;d@�
=@���@�v�@�5?@��@���@���@��@��@���@���@�z�@�1'@��;@���@�dZ@�33@�o@�
=@��y@�ȴ@�n�@��T@���@���@�p�@�&�@���@�Q�@�9X@�(�@�b@�1@�  @��m@��F@��@�\)@��K@�{�@u�Z@j��@_>�@X7�@P�_@H[�@A�@<��@5��@1��@+�Q@%�@!V@��@ �@PH@5?@�M@J#11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   Aִ9A֮Aְ!Aְ!AֶFAּjAּjAָRA֙�A�z�A�bAՏ\A�O�A��A���A��;A���AԺ^Aԥ�AԁA��A�t�A�Aџ�AθRA��#A���A��AƁA�VA���A���A�ƨA�bA�?}A�n�A�n�A���A�33A���A�XA��A��A�-A�=qA�A�XA���A�%A�ƨA��mA��9A��A�^5A�z�A��DA�JA�&�A���A�A��wA�z�A��!A���A��A���A��9A���A�/A��
A�O�A�XA�
=A���A�|�A��\A�v�A���A�+A���A��;A�z�A���A�-A�JA���A���A���A��A�1'A��A�z�A��hA��A���A��TA���A�z�A�dZA�/A�O�AG�A|AyhsAw��Av�At�Ar5?Ap1'Ak��Ai�AgC�AfA�Af �Adr�A^z�A[�A[�AXĜAW��AV-AUdZAS�TAQ�-AQdZAP�ALJAIt�AH�AH��AG�FAF�jAF1'AE�7AD��ABn�AA�wAAoA@��A@�jA@jA?dZA=�
A=&�A;��A;S�A:�!A:E�A9��A9p�A9�A8ĜA8A�A7�TA6��A3A2ZA1�A0�jA0-A/hsA.��A-�A,�+A+?}A*ZA(�A'��A';dA&�\A&�A%�A$JA#��A#O�A"��A��A{A��A;dA�`A5?A1A�jA�#AG�A��A��A��AZA+A1'AbNA��At�A\)A/A�A��A�hAdZA�A
=A�A�+A�-A��Ar�A9XA��A
 �A��A�^A��AA�hAK�A�uA�A�A �HA J@���@�1@��!@�x�@��/@�r�@�S�@��@�^@�(�@���@�+@�(�@�S�@�?}@�ƨ@���@�$�@�1'@�O�@��@�n�@�hs@��
@ڧ�@��@�X@؛�@��;@թ�@�A�@�
=@�J@���@�
=@Η�@�%@��m@˕�@�t�@�+@���@�ff@�V@�5?@�@�@��@��#@���@ƸR@�%@ċD@�9X@�|�@���@�G�@���@��9@���@�I�@��w@���@�j@�"�@��\@��+@��\@��+@��+@�~�@�~�@�~�@�~�@�V@��@�X@��@��`@�j@�9X@��@��
@�dZ@��\@�{@���@�x�@�Ĝ@�(�@��P@��\@�ff@�@�x�@�X@�/@��`@��D@�bN@�Q�@�Z@���@���@��@��w@�t�@��@��7@�G�@��u@�  @���@�+@��!@�n�@��@�hs@�  @��y@�ff@��@���@���@���@��7@�?}@�?}@��@���@��D@��@��F@�C�@�o@��!@�M�@�{@���@��#@��#@��T@��@��@�v�@��!@�v�@��@���@�&�@�%@�bN@�(�@��w@�S�@���@�$�@�@�X@��`@�9X@�ƨ@���@�S�@�"�@�ȴ@�n�@�M�@�=q@�J@��T@���@���@��7@�x�@��@��@�Ĝ@���@��D@�j@�(�@�b@��m@�S�@��R@��+@�^5@�E�@�E�@�-@�J@��@��^@��-@��^@��^@��-@��^@��^@��@��u@���@���@�%@���@��`@���@�bN@�Z@� �@���@��@�l�@�+@�ȴ@�ȴ@��!@�n�@�@�?}@��`@��/@���@�A�@��F@���@�;d@�
=@���@�v�@�5?@��@���@���@��@��@���@���@�z�@�1'@��;@���@�dZ@�33@�o@�
=@��y@�ȴ@�n�@��T@���@���@�p�@�&�@���@�Q�@�9X@�(�@�b@�1@�  @��m@��F@��G�O�@��K@�{�@u�Z@j��@_>�@X7�@P�_@H[�@A�@<��@5��@1��@+�Q@%�@!V@��@ �@PH@5?@�M@J#11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB.B.B.B.B0!B33B33BD�Be`By�B�3B��B%�BE�BP�BR�BS�BS�BS�BS�BR�BR�BP�BO�BZB\)BYBdZBp�Br�Bw�B{�B�B�B�B�JB�bB�oB�hB�\B�PB�DB�7B�B�B�B�+B�7B�PB��B��B��B��B��B��BÖB��BǮB�dB�}BÖB��B��B��BȴBǮBŢBÖB�wB��Bs�BS�BG�B9XB.B'�B!�B�B\BB��B��B�LB��B��B�B�dB��B�LB�B\)B49B�BoB
��B
��B
�LB
��B
��B
�B
t�B
P�B
6FB
"�B
�B
JB
  B	�B	�`B	��B	��B	��B	ĜB	ȴB	ĜB	��B	�B	�B	s�B	y�B	r�B	jB	\)B	G�B	E�B	A�B	'�B	�B	�B	�B	bB	DB		7B	%B	B	  B	B	
=B	JB	JB		7B	B��B��B��B�B�B�B�B�sB�fB�ZB�HB�/B�B��BƨBÖB��B�}B�wB�dB�FB�-B�B�B�B�B�B��B��B��B��B��B��B��B�hB�\B�VB�JB�DB�1B�B�B� B}�B|�B{�B{�Bx�Bu�Br�Bp�Bn�Bn�Bm�Bl�Bk�Bk�BjBjBhsBhsBgmBe`BcTBbNBaHB`BB^5B[#B[#BffBn�Bo�Bn�Bn�Bl�Bk�Bk�BjBiyBhsBgmBgmBgmBgmBffBe`BdZBffBffBe`BdZBffBffBhsBjBjBiyBk�Bp�Br�Br�Br�Bs�Bt�Bt�Bt�Bt�Bs�Bv�By�B{�B|�B~�B~�B~�B�B�B�B�B�B�B�B�B�B�%B�B�B�B�%B�bB��B��B��B��B�B�B�!B�!B�!B�'B�3B�jB��BȴB��B��B��B��B��B��B��B��B��B��B��B�B�B�B�)B�/B�5B�5B�BB�NB�`B�fB�sB�B�B�B�B�B��B��B��B��B��B��B	  B	B	B	DB	JB	PB	\B	bB	uB	�B	�B	�B	!�B	%�B	(�B	.B	0!B	49B	6FB	@�B	H�B	L�B	P�B	R�B	R�B	S�B	T�B	W
B	YB	[#B	\)B	`BB	bNB	dZB	ffB	hsB	k�B	n�B	q�B	v�B	w�B	w�B	x�B	x�B	z�B	~�B	�B	�+B	�JB	�PB	�oB	�{B	�uB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�'B	�'B	�-B	�3B	�?B	�FB	�FB	�LB	�XB	�XB	�^B	�^B	�dB	�wB	�}B	��B	��B	B	ÖB	ŢB	ƨB	ƨB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�
B	�
B	�B	�#B	�#B	�/B	�;B	�TB	�fB	�fB	�fB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
+B
1B
	7B
	7B
	7B

=B
DB
JB
PB
PB
PB
PB
PB
PB
VB
VB
\B
�B
~B
($B
2�B
=B
A�B
H�B
N�B
T�B
X_B
]B
`BB
d&B
j0B
n�B
s�B
x�B
|PB
~�B
��B
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B%*B%.B%-B%)B'8B*FB*DB;�B\sBp�B�@B��B�B<�BG�BI�BJ�BJ�BJ�BJ�BI�BI�BG�BF�BQ BS.BPB[`Bg�Bi�Bn�Br�B|"B|$B|%B�LB�hB�qB�kB�XB�SB�GB�;B{BxB|%B~-B�;B�WB��B��B��B��B��B��B��B��B��B�dB�{B��B��B��B��B��B��B��B��B��B��Bj�BKB>�B0hB%%B�B�B�BkB�1B��B�B�gB��B�B�B�}B��B�eBz/BSKB+^B�B	�B
�B
�+B
�zB
�B
��B
{IB
k�B
HB
-zB
B
�B
�B	�=B	��B	ܝB	�B	�B	��B	��B	��B	��B	��B	{^B	|cB	j�B	q$B	i�B	a�B	SvB	>�B	<�B	8�B	?B	�B	�B	�B	�B	�B	 �B�vB�eB�TB�nB	�B	�B	�B	 �B�kB�EB�5B�B�
B��B��B��B��BݼB۵B؞BԂB�[B�B�B��B��B��B��B��B��B��B�qB�pB�kB�kB�^B�TB�>B�.B�!B�B�B��B��B��B��B��B��B�B|�ByoBw`BuXBtRBsJBsKBp7Bm$BjBhBe�Be�Bd�Bc�Bb�Bb�Ba�Ba�B_�B_�B^�B\�BZ�BY�BX�BW�BU�BR�BR�B]�Be�BgBe�Be�Bc�Bb�Bb�Ba�B`�B_�B^�B^�B^�B^�B]�B\�B[�B]�B]�B\�B[�B]�B]�B_�Ba�Ba�B`�Bb�BhBjBjBjBkBl Bl Bl"Bl%BkBn-Bq?BsOBtVBvcBv^BvaByuB{~B{�B{�B{B|�B|�B|�B|�B}�B|�B|�B{}B}�B��B�B�$B�,B�EB�mB�wB��B��B��B��B��B��B��B�B�7B�7B�6B�4B�4B�<B�<B�>B�>B�MB�[B�tB�B�}BӎBԓBՔBՙBץBٱB��B��B��B��B��B��B�B�B�#B�DB�HB�DB�PB�ZB�`B�gB�xB	�B	�B	�B	�B	�B	
�B	�B	�B	B	+B	?B	 UB	%rB	'~B	+�B	-�B	7�B	@B	D&B	HAB	JPB	JOB	KTB	L\B	NhB	PrB	RB	S�B	W�B	Y�B	[�B	]�B	_�B	b�B	e�B	iB	n#B	o)B	o)B	p-B	p2B	r9B	vSB	{rB	~�B	��B	��B	��B	��B	��B	��B	��B	�B	�#B	�(B	�-B	�5B	�BB	�`B	�wB	�~B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	��B	�B	�.B	�9B	�BB	�@B	�@B	�AB	�GB	�MB	�XB	�XB	�]B	�dB	�_B	�bB	�xB	�xB	ԀB	֑B	ڧB	ݹB	ݷB	ݺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�B	�B	�"B	�"B	�,B	�.B	�<B	�?B	�>B	�FB	�OB	�NB	�LB	�UB	�YB	�XB	�VB	�^B	�eB	�oB	�nB	�qB	�yB	�uB	�xB	�uB	�|B	��B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�G�O�B
�B
�B
vB
*B
4UB
9)B
@7B
F&B
K�B
O�B
TdB
W�B
[vB
a�B
fB
j�B
p%B
s�B
u�B
z�B
|�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.12 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0), vertically averaged dS =-0.009(+/-0.001) in PSS-78.                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Map scales: x=6,3; y=2,1. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                            201906040940282019060409402820190604094028  AO  ARCAADJP                                                                    20170609170306    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20170609170306  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20170609170306  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604094028  IP                  G�O�G�O�G�O�                