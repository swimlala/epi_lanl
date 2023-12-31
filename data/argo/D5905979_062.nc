CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:09:09Z creation      
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
_FillValue                    �`Argo profile    3.1 1.2 19500101000000  20200619170909  20220204114417  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               >A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @ث�I�"1   @ث�{Bm�@6���"���c�\(�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    >A   B   B   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B/��B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cu�fCx  Cz  C|  C~  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D fD � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN�fDO  DO� DP  DP� DQ  DQ� DR  DR�fDS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dts3Dy��D�RD�Y�D��)D��=D�qD�T�D��)D�� D��D�T�D��\D��3D�${D�Y�Dڧ\D��
D�!�D�Z�D�\D��q11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@�(�@�(�A{A>{A^{A~{A�
=A�
=A�
=A�
=A�
=A�
=A�
=A�
=B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B���B�B�B�B�C�HC�HC�HC�HC	�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC!�HC#�HC%�HC'�HC)�HC+�HC-�HC/�HC1�HC3�HC5�HC7�HC9�HC;�HC=�HC?�HCA�HCC�HCE�HCG�HCI�HCK�HCM�HCO�HCQ�HCS�HCU�HCW�HCY�HC[�HC]�HC_�HCa�HCc�HCe�HCg�HCi�HCk�HCm�HCo�HCq�HCs�HCuǮCw�HCy�HC{�HC}�HC��C��qC��qC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D xRD �RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RD	xRD	�RD
xRD
�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD��D xRD �RD!xRD!�RD"xRD"�RD#xRD#�RD$xRD$�RD%xRD%�RD&xRD&�RD'xRD'�RD(xRD(�RD)xRD)�RD*xRD*�RD+xRD+�RD,xRD,�RD-xRD-�RD.xRD.�RD/xRD/�RD0xRD0�RD1xRD1�RD2xRD2�RD3xRD3�RD4xRD4�RD5xRD5�RD6xRD6�RD7xRD7�RD8xRD8�RD9xRD9�RD:xRD:�RD;xRD;�RD<xRD<�RD=xRD=�RD>xRD>�RD?xRD?�RD@xRD@�RDAxRDA�RDBxRDB�RDCxRDC�RDDxRDD�RDExRDE�RDFxRDF�RDGxRDG�RDHxRDH�RDIxRDI�RDJxRDJ�RDKxRDK�RDLxRDL�RDMxRDM�RDN~�DN�RDOxRDO�RDPxRDP�RDQxRDQ�RDR~�DR�RDSxRDS�RDTxRDT�RDUxRDU�RDVxRDV�RDWxRDW�RDXxRDX�RDYxRDY�RDZxRDZ�RD[xRD[�RD\xRD\�RD]xRD]�RD^xRD^�RD_xRD_�RD`xRD`�RDaxRDa�RDbxRDb�RDcxRDc�RDdxRDd�RDexRDe�RDfxRDf�RDgxRDg�RDhxRDh�RDixRDi�RDjxRDj�RDkxRDk�RDlxRDl�RDmxRDm�RDnxRDn�RDoxRDo�RDpxRDp�RDqxRDq�RDrxRDr�RDsxRDs�RDtk�Dy��D�{D�U�D��RD��fD��D�P�D��RD��)D� D�P�D���D��\D� �D�VDڣ�D��3D��D�W
D�D�њ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��-A��9A��wA��jA��jA��^A��^A��wA��wA�ĜA�ƨA��wA�ƨA�ƨA�ƨA���A���A���A���A�ĜA��^A���A��wA�ȴA��FA���A���A��hA��+A��DA��uA���A��PA�v�A�n�A�\)A�1'A�JA��A��A���A��#A��FA���A��PA�p�A���A�p�A���A��A��HA�v�A�A���A�(�A�XA�oA��;A�A�%A���A��A�Q�A�1A�1'A�%A���A��jA���A�VA��A���A�dZA� �A���A�bNA��A���A�l�A���A�G�A��A�l�A��FA� �A��-A���A��A�v�A���A���A�^5A��A�%A�v�A��A�ffA�A��A�n�A��jA���A��mA��A��jA�$�A}%A|9XA{�AyƨAx�Ax �Aw��Aw|�AwhsAv�\Ar��AqG�AmXAh�/Af-Ad~�Ab��A`z�A_�A^��A\�uA[hsAZ~�AX�AWl�AT�AS�AQ;dAP�HAPbNAO;dAL�AK`BAJ�RAJ�AJbAF�`AE|�ADz�AA��A?O�A>r�A=�A=dZA=&�A;�
A9O�A7VA6�A5�mA3�;A2�/A2n�A2-A1��A1"�A0�A0ffA.��A+��A*r�A(�HA((�A'�TA'��A'K�A'"�A&��A&�9A&JA$-A#K�A!l�A!�A �A I�A�^A\)AC�AȴA��A��A=qAO�Ar�A��AbNAE�AC�A��Av�A�;Al�A%A�\A�TA�yA�hA�#A��A
��A	O�A�A(�A;dA��A�!Ax�A�\AbAG�A�A��AXA�A �yA ��@��@�ff@�o@�7L@�;d@�\@��#@�j@�  @�dZ@�
=@���@�r�@�
=@�7@�t�@�1'@��@�\@�V@�G�@��@�C�@ڸR@�^5@�G�@׾w@�\)@��H@�hs@�z�@�b@Ӿw@�V@д9@��m@Ϯ@υ@Ο�@͡�@�%@�Z@˾w@�|�@ʧ�@ȣ�@�;d@Ɨ�@ċD@���@��@�ff@��@���@��#@��h@�7L@��
@�@� �@�33@��+@�M�@�-@���@�x�@���@�1'@�\)@��@���@��`@�A�@��F@�S�@���@��@�@���@�G�@���@��j@�Q�@�b@�  @�ƨ@�C�@�~�@��#@�G�@��@��/@��F@�dZ@�K�@�C�@�;d@���@��@��@�O�@���@���@��;@��P@�S�@�
=@���@�V@�-@�{@�J@���@���@��h@��7@���@�X@��`@�&�@��@��w@�ƨ@��;@���@��@��@��@��
@�S�@�$�@��-@�x�@��@��`@��j@�z�@�A�@�1'@��;@��w@���@�t�@�33@�@���@���@�ff@�V@�-@�@��@��#@���@��7@��@�p�@�`B@��@��D@�j@�9X@� �@�1@��;@��@�t�@�+@��y@���@���@��R@���@�@�$�@�@���@���@�`B@��@��@��u@��@�r�@�Z@��@���@�\)@�C�@�33@��@�@���@��\@�M�@�{@�@��h@�`B@��@���@��D@�Z@�A�@���@���@�l�@�K�@�+@�o@���@�M�@�-@��#@�x�@�/@�V@��@��9@�I�@��P@��@���@��@�p�@�`B@�&�@���@��9@�Z@��w@���@�\)@���@��R@�-@���@�Q�@�A�@���@�7L@�/@��@��@���@��j@��u@�Z@�1@��m@��w@�l�@�K�@�;d@�@���@�~�@�ff@�^5@�=q@��@�@���@���@��-@�
�@�6@v��@he�@^^5@WMj@O8@G��@?�@9B�@4��@-A @&�'@"�@c�@��@�r@2�@-�@��@�F11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A��-A��9A��wA��jA��jA��^A��^A��wA��wA�ĜA�ƨA��wA�ƨA�ƨA�ƨA���A���A���A���A�ĜA��^A���A��wA�ȴA��FA���A���A��hA��+A��DA��uA���A��PA�v�A�n�A�\)A�1'A�JA��A��A���A��#A��FA���A��PA�p�A���A�p�A���A��A��HA�v�A�A���A�(�A�XA�oA��;A�A�%A���A��A�Q�A�1A�1'A�%A���A��jA���A�VA��A���A�dZA� �A���A�bNA��A���A�l�A���A�G�A��A�l�A��FA� �A��-A���A��A�v�A���A���A�^5A��A�%A�v�A��A�ffA�A��A�n�A��jA���A��mA��A��jA�$�A}%A|9XA{�AyƨAx�Ax �Aw��Aw|�AwhsAv�\Ar��AqG�AmXAh�/Af-Ad~�Ab��A`z�A_�A^��A\�uA[hsAZ~�AX�AWl�AT�AS�AQ;dAP�HAPbNAO;dAL�AK`BAJ�RAJ�AJbAF�`AE|�ADz�AA��A?O�A>r�A=�A=dZA=&�A;�
A9O�A7VA6�A5�mA3�;A2�/A2n�A2-A1��A1"�A0�A0ffA.��A+��A*r�A(�HA((�A'�TA'��A'K�A'"�A&��A&�9A&JA$-A#K�A!l�A!�A �A I�A�^A\)AC�AȴA��A��A=qAO�Ar�A��AbNAE�AC�A��Av�A�;Al�A%A�\A�TA�yA�hA�#A��A
��A	O�A�A(�A;dA��A�!Ax�A�\AbAG�A�A��AXA�A �yA ��@��@�ff@�o@�7L@�;d@�\@��#@�j@�  @�dZ@�
=@���@�r�@�
=@�7@�t�@�1'@��@�\@�V@�G�@��@�C�@ڸR@�^5@�G�@׾w@�\)@��H@�hs@�z�@�b@Ӿw@�V@д9@��m@Ϯ@υ@Ο�@͡�@�%@�Z@˾w@�|�@ʧ�@ȣ�@�;d@Ɨ�@ċD@���@��@�ff@��@���@��#@��h@�7L@��
@�@� �@�33@��+@�M�@�-@���@�x�@���@�1'@�\)@��@���@��`@�A�@��F@�S�@���@��@�@���@�G�@���@��j@�Q�@�b@�  @�ƨ@�C�@�~�@��#@�G�@��@��/@��F@�dZ@�K�@�C�@�;d@���@��@��@�O�@���@���@��;@��P@�S�@�
=@���@�V@�-@�{@�J@���@���@��h@��7@���@�X@��`@�&�@��@��w@�ƨ@��;@���@��@��@��@��
@�S�@�$�@��-@�x�@��@��`@��j@�z�@�A�@�1'@��;@��w@���@�t�@�33@�@���@���@�ff@�V@�-@�@��@��#@���@��7@��@�p�@�`B@��@��D@�j@�9X@� �@�1@��;@��@�t�@�+@��y@���@���@��R@���@�@�$�@�@���@���@�`B@��@��@��u@��@�r�@�Z@��@���@�\)@�C�@�33@��@�@���@��\@�M�@�{@�@��h@�`B@��@���@��D@�Z@�A�@���@���@�l�@�K�@�+@�o@���@�M�@�-@��#@�x�@�/@�V@��@��9@�I�@��P@��@���@��@�p�@�`B@�&�@���@��9@�Z@��w@���@�\)@���@��R@�-@���@�Q�@�A�@���@�7L@�/@��@��@���@��j@��u@�Z@�1@��m@��w@�l�@�K�@�;d@�@���@�~�@�ff@�^5@�=q@��@�@���@���G�O�@�
�@�6@v��@he�@^^5@WMj@O8@G��@?�@9B�@4��@-A @&�'@"�@c�@��@�r@2�@-�@��@�F11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�!B�FB�RB�XB�dB��B��B�ZB�B��B��B��B��B��B��B	7B�B�B�B�B'�B6FB:^B9XB9XB=qB'�B�B�B$�B#�B�B�B�B�B{BuBhBJBJBDBJB
=BbBJB+BB  B��B��B�mBƨB��B�hB�B{�Bv�Bq�BhsBaHB[#BN�B$�BB
�B
�B
��B
�wB
��B
��B
��B
�DB
}�B
v�B
iyB
M�B
>wB
2-B
-B
#�B
�B
1B
B	��B	�B	�B	�fB	�NB	�BB	�;B	�#B	ÖB	�-B	��B	|�B	cTB	XB	N�B	>wB	6FB	49B	+B	%�B	 �B	�B	VB��B��B�B��B	  B	B��B��B��B��B��B�B�mB�HB�BƨB��B�}B�qB�}BB�LB�B�B�B��B��B��B��B��B�B�B�B��B��B�VB�=B�B�B�B�B� B� B~�B{�Bu�Bo�BiyBdZBcTB`BB^5B[#B[#BYBW
BR�BP�BO�BL�BI�BF�B@�B>wB<jB<jB;dB9XB8RB7LB6FB5?B49B2-B1'B.B/B,B-B+B)�B)�B)�B'�B&�B(�B&�B&�B&�B%�B%�B#�B$�B&�B$�B$�B$�B#�B#�B$�B#�B#�B"�B#�B"�B#�B#�B%�B&�B&�B&�B%�B'�B+B/B.B.B0!B2-B2-B2-B5?B5?B6FB6FB9XB<jB=qB=qB=qB?}BA�BC�BD�BF�BF�BG�BO�BQ�BS�B[#B\)B`BBdZBffBgmBhsBjBk�Bm�Br�Bz�B�B�%B�+B�1B�7B�JB�\B�{B��B��B��B��B��B��B��B�B�B�B�-B�9B�FB�LB�RB�dB�jB�wB��BÖBŢBǮBǮB��B�B�B�B�B�#B�)B�BB�TB�ZB�ZB�`B�yB�B�B�B�B��B��B��B��B��B��B��B��B��B	  B	  B	B		7B	JB	VB	\B	hB	oB	oB	uB	�B	�B	'�B	+B	+B	/B	2-B	33B	49B	6FB	7LB	;dB	;dB	<jB	=qB	@�B	B�B	D�B	F�B	H�B	H�B	J�B	K�B	L�B	L�B	N�B	P�B	P�B	P�B	Q�B	S�B	ZB	[#B	]/B	^5B	_;B	aHB	cTB	ffB	jB	o�B	s�B	w�B	y�B	z�B	�B	�+B	�JB	�PB	�\B	�bB	�uB	�uB	�{B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�-B	�-B	�?B	�FB	�LB	�RB	�^B	�dB	�dB	�jB	�jB	�}B	��B	��B	B	ĜB	ƨB	ǮB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�
B	�
B	�B	��B	��B	�B	�#B	�;B	�HB	�HB	�NB	�NB	�ZB	�ZB	�ZB	�`B	�`B	�fB	�mB	�mB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�oB	��B
gB
(B
�B
'�B
4B
:xB
B'B
H1B
K�B
SB
YB
]/B
a|B
gB
k�B
p;B
t�B
z*B
}11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�(B�MB�XB�^B�jB��B��B�^B�B�B��B��B��B��B��B9B�B�B�B�B�B.EB2]B1XB1XB5qB�B�B�B�B�B�B�B�B�BByB	lBOBOBIBOBBBgBOB�1B�B�B��B��B�uB��B��B�vBzBs�Bn�Bi�B`�BYZBS5BF�B�B
�*B
�B
�2B
�B
��B
�B
��B
��B
�eB
vB
n�B
a�B
E�B
6�B
*TB
%6B
�B
�B
 [B	�7B	�B	��B	�B	ޓB	�{B	�oB	�hB	�QB	��B	�^B	��B	u#B	[�B	PGB	GB	6�B	.�B	,sB	#=B	B	B	�B	�B�8B�B��B�B�?B�QB�9B�!B�B�B�B��B߮BيB�SB��B��B��B��B��B��B��B�ZB�aB�ZB�B��B��B�B�7B�UB�[B�UB�%B��B��B��B}iB|dB{]BzWBxKBxKBwEBt2BnBg�Ba�B\�B[�BX�BV�BSqBSqBQeBOXBKABI4BH.BEBB
B>�B8�B6�B4�B4�B3�B1�B0�B/�B.�B-�B,�B*B)zB&gB'nB$[B%aB#VB"PB"PB"PB DB=B!JB>B>B>B8B8B,B2B>B3B3B3B-B-B3B-B-B'B-B'B-B-B9B@B@B@B:B GB#YB'rB&kB&kB(xB*�B*�B*�B-�B-�B.�B.�B1�B4�B5�B5�B5�B7�B9�B;�B<�B>�B>�B@BH5BJBBLNBSyBTBX�B\�B^�B_�B`�Bb�Bc�Be�BkBs5Bz`B~yBB��B��B��B��B��B��B��B�B�/B�;B�HB�HB�TB�mB�mB�B��B��B��B��B��B��B��B��B��B��B��B��B�B�`B�`B�gB�mB�sB�yBؑBۣBܩBܩBݯB��B��B��B��B��B�B�B�B�B�B�)B�/B�5B�BB�NB�NB�ZB	�B	�B	�B	�B		�B	
�B	
�B	�B	�B	B	 <B	#NB	#NB	'fB	*xB	+~B	,�B	.�B	/�B	3�B	3�B	4�B	5�B	8�B	:�B	<�B	>�B	@�B	@�B	CB	DB	EB	EB	G#B	I/B	I/B	I/B	J6B	LAB	RfB	SlB	UxB	V~B	W�B	Y�B	[�B	^�B	b�B	g�B	k�B	pB	r"B	s(B	yMB	rB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�"B	�.B	�AB	�GB	�SB	�YB	�fB	�qB	�qB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�(B	�(B	�5B	�5B	�;B	�AB	�GB	�MB	�SB	�GB	�GB	�GB	�MB	�MB	�GB	�;B	�5B	�GB	�fB	�~B	يB	يB	ڐB	ڐB	ܜB	ܜB	ܜB	ݢB	ݢB	ިB	߯B	߯B	߯B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��G�O�B	�B	��B	��B
iB
�B
�B
,_B
2�B
:fB
@pB
DB
KKB
QVB
UnB
Y�B
_BB
dB
hyB
mB
rhB
w�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.12 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.008(+/-0.004) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144172022020411441720220204114417  AO  ARCAADJP                                                                    20200619170909    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170909  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170909  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114417  IP                  G�O�G�O�G�O�                