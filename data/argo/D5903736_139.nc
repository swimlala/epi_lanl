CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-02-14T03:15:22Z AOML 3.0 creation; 2016-05-31T19:14:47Z UW 3.1 conversion     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20160214031522  20190604093959  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051_7090_139                   2C  D   APEX                            5368                            041511                          846 @וxW
Y�1   @וx�Ʃ@4̋C���dZȴ9X1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   A   A   @@  @�  @�  A   A   A@  A`  A�  A�  A�  A�  A���A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DFy�DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDy�fD� D�VfD�s3D�ٚD� D�C3D�� D�� D� D�,�D��3D�� D��3D�\�Dڌ�D��D���D�  D�Y�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @8Q�@xQ�@�(�@�(�A{A>{A^{A~{A�
=A�
=A�
=A��
A�
=A�
=A�
=A�
=B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B�B�B�B���B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C�HC�HC�HC�HC	�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC!�HC#�HC%�HC'�HC)�HC+�HC-�HC/�HC1�HC3�HC5�HC7�HC9�HC;�HC=�HC?�HCA�HCC�HCE�HCG�HCI�HCK�HCM�HCO�HCQ�HCS�HCU�HCW�HCY�HC[�HC]�HC_�HCa�HCc�HCe�HCg�HCi�HCk�HCm�HCo�HCq�HCs�HCu�HCw�HCy�HC{�HC}�HC�HC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D xRD �RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RD	xRD	�RD
xRD
�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RD xRD �RD!xRD!�RD"xRD"�RD#xRD#�RD$xRD$�RD%xRD%�RD&xRD&�RD'xRD'�RD(xRD(�RD)xRD)�RD*xRD*�RD+xRD+�RD,xRD,�RD-xRD-�RD.xRD.�RD/xRD/�RD0xRD0�RD1xRD1�RD2xRD2�RD3xRD3�RD4xRD4�RD5xRD5�RD6xRD6�RD7xRD7�RD8xRD8�RD9xRD9�RD:xRD:�RD;xRD;�RD<xRD<�RD=xRD=�RD>xRD>�RD?xRD?�RD@xRD@�RDAxRDA�RDBxRDB�RDCxRDC�RDDxRDD�RDExRDE�RDFq�DF�RDGxRDG�RDHxRDH�RDIxRDI�RDJxRDJ�RDKxRDK�RDLxRDL�RDMxRDM�RDNxRDN�RDOxRDO�RDPxRDP�RDQxRDQ�RDRxRDR�RDSxRDS�RDTxRDT�RDUxRDU�RDVxRDV�RDWxRDW�RDXxRDX�RDYxRDY�RDZxRDZ�RD[xRD[�RD\xRD\�RD]xRD]�RD^xRD^�RD_xRD_�RD`xRD`�RDaxRDa�RDbxRDb�RDcxRDc�RDdxRDd�RDexRDe�RDfxRDf�RDgxRDg�RDhxRDh�RDixRDi�RDjxRDj�RDkxRDk�RDlxRDl�RDmxRDm�RDnxRDn�RDoxRDo�RDpxRDp�RDqxRDq�RDrxRDr�RDsxRDs�RDtxRDt޸Dy��D�)D�R�D�o\D���D�)D�?\D�|)D��)D�)D�(�D��\D��)D��\D�X�Dڈ�D��D���D�)D�U�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�9XA�;dA�9XA�-A�(�A�(�A�&�A�&�A�-A�Q�A�A�ȴA�bNA� �A��A�{A�bA�%A��mA�5?AǅA�I�A�S�A�  A�
=A�ffA���A+A��A��
A�ȴA��FA��9A�jA��wA�=qA�oA�^5A��A�C�A�&�A��`A���A�M�A��-A� �A���A��A�t�A���A�A�G�A��jA�VA�ZA���A�l�A���A�E�A�x�A�^5A�bA��`A��A�x�A���A��PA�ZA��jA�1'A�K�A��A��!A��A��A��PA��\A���A���A��A���A���A��A���A�A�ffA���A�{A���A�v�A�v�A�A�|�A��-A���A�33A�;dA��jA�x�A��A��7A��^A�ffA�+A���A���A�ƨA�$�A���A���A���A���A��+A��A�A}33A|A�Az�yAy��AxZAu�;At-As%Ap�DAn��Am�wAl^5Ak�Aj(�Ahz�Ag�Af��Af �Ac�TA_S�A];dA\^5A\bA[�FAZE�AX�RAWC�AVn�AU\)AT  ASoAR(�AP�\AP�AO��AN�/AL��AK
=AJ5?AIl�AH5?AFz�AE%AB�AA�A?��A>��A=��A=�A<��A;�wA:9XA6M�A4n�A1�A0��A/&�A-A+�FA)��A)A)�-A)x�A(�A(1A'|�A&�A$r�A"��A"~�A!��A!VA�#A�AjA�A�A��A��A%A�/Av�A��AG�A�AjAbA��A�A  AQ�Al�A��A�HA-A�A
��A�A�TA\)A�A�A�hAA ��@�"�@�?}@��@��@���@�V@�
=@��@�?}@�Ĝ@�9X@�|�@��@�@��@��m@�@�1'@�E�@�&�@�j@��@ߕ�@ޗ�@�(�@۾w@��@ٲ-@�b@׾w@��@Դ9@�ff@�`B@Л�@�S�@Ο�@�n�@�E�@ͩ�@�%@̣�@�z�@�1@�dZ@�ȴ@ɩ�@���@���@��@Ƈ+@�5?@őh@�V@�1@�|�@��y@�V@�{@���@�G�@�&�@�&�@���@��u@���@���@��@��@�{@�X@�V@���@���@�(�@�ƨ@��P@��@��R@�v�@�=q@�@�@��@�?}@��D@��@�C�@�@�~�@�5?@���@���@���@��@�&�@��/@��j@��u@�ƨ@���@��@�^5@���@��9@�r�@�I�@��;@�+@�5?@���@�&�@��j@�(�@��P@��@��@���@���@�5?@���@���@��@��@��@�x�@��`@���@���@��@�K�@��H@��\@�M�@�$�@��#@���@��7@��`@��9@�9X@�ƨ@�C�@�33@�K�@�-@���@�x�@��^@��@�@���@��-@��h@�X@���@��9@��D@�I�@��@��@��w@�dZ@���@��+@��\@�ff@�J@��@�/@�V@��`@���@�Ĝ@��j@��@��m@�dZ@�o@�V@��@���@���@���@��h@��h@�p�@�?}@���@�z�@���@���@��P@�|�@�33@�S�@�
=@���@�~�@�$�@��-@�hs@��`@�%@���@�z�@�A�@� �@��;@��w@��P@�33@�@�ȴ@�ff@��T@���@�p�@�G�@��@��/@���@��j@��u@�z�@��@��@�I�@��F@�\)@�\)@�K�@�;d@��@��!@��+@��@�x�@�O�@�V@��`@��9@���@��u@�j@�9X@��@��m@�|�@�K�@��R@��+@��+@�V@���@�/@��/@��9@��u@��@�z�@�(�@��m@��w@���@�
=@���@�^5@��@���@�\)@|I�@q��@h��@`�9@V�@O�P@G�@BM�@;�
@7\)@0�u@+�@'�P@"�@{@��@/@  @dZ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�9XA�;dA�9XA�-A�(�A�(�A�&�A�&�A�-A�Q�A�A�ȴA�bNA� �A��A�{A�bA�%A��mA�5?AǅA�I�A�S�A�  A�
=A�ffA���A+A��A��
A�ȴA��FA��9A�jA��wA�=qA�oA�^5A��A�C�A�&�A��`A���A�M�A��-A� �A���A��A�t�A���A�A�G�A��jA�VA�ZA���A�l�A���A�E�A�x�A�^5A�bA��`A��A�x�A���A��PA�ZA��jA�1'A�K�A��A��!A��A��A��PA��\A���A���A��A���A���A��A���A�A�ffA���A�{A���A�v�A�v�A�A�|�A��-A���A�33A�;dA��jA�x�A��A��7A��^A�ffA�+A���A���A�ƨA�$�A���A���A���A���A��+A��A�A}33A|A�Az�yAy��AxZAu�;At-As%Ap�DAn��Am�wAl^5Ak�Aj(�Ahz�Ag�Af��Af �Ac�TA_S�A];dA\^5A\bA[�FAZE�AX�RAWC�AVn�AU\)AT  ASoAR(�AP�\AP�AO��AN�/AL��AK
=AJ5?AIl�AH5?AFz�AE%AB�AA�A?��A>��A=��A=�A<��A;�wA:9XA6M�A4n�A1�A0��A/&�A-A+�FA)��A)A)�-A)x�A(�A(1A'|�A&�A$r�A"��A"~�A!��A!VA�#A�AjA�A�A��A��A%A�/Av�A��AG�A�AjAbA��A�A  AQ�Al�A��A�HA-A�A
��A�A�TA\)A�A�A�hAA ��@�"�@�?}@��@��@���@�V@�
=@��@�?}@�Ĝ@�9X@�|�@��@�@��@��m@�@�1'@�E�@�&�@�j@��@ߕ�@ޗ�@�(�@۾w@��@ٲ-@�b@׾w@��@Դ9@�ff@�`B@Л�@�S�@Ο�@�n�@�E�@ͩ�@�%@̣�@�z�@�1@�dZ@�ȴ@ɩ�@���@���@��@Ƈ+@�5?@őh@�V@�1@�|�@��y@�V@�{@���@�G�@�&�@�&�@���@��u@���@���@��@��@�{@�X@�V@���@���@�(�@�ƨ@��P@��@��R@�v�@�=q@�@�@��@�?}@��D@��@�C�@�@�~�@�5?@���@���@���@��@�&�@��/@��j@��u@�ƨ@���@��@�^5@���@��9@�r�@�I�@��;@�+@�5?@���@�&�@��j@�(�@��P@��@��@���@���@�5?@���@���@��@��@��@�x�@��`@���@���@��@�K�@��H@��\@�M�@�$�@��#@���@��7@��`@��9@�9X@�ƨ@�C�@�33@�K�@�-@���@�x�@��^@��@�@���@��-@��h@�X@���@��9@��D@�I�@��@��@��w@�dZ@���@��+@��\@�ff@�J@��@�/@�V@��`@���@�Ĝ@��j@��@��m@�dZ@�o@�V@��@���@���@���@��h@��h@�p�@�?}@���@�z�@���@���@��P@�|�@�33@�S�@�
=@���@�~�@�$�@��-@�hs@��`@�%@���@�z�@�A�@� �@��;@��w@��P@�33@�@�ȴ@�ff@��T@���@�p�@�G�@��@��/@���@��j@��u@�z�@��@��@�I�@��F@�\)@�\)@�K�@�;d@��@��!@��+@��@�x�@�O�@�V@��`@��9@���@��u@�j@�9X@��@��m@�|�@�K�@��R@��+@��+@�V@���@�/@��/@��9@��u@��@�z�@�(�@��m@��w@���@�
=@���@�^5@��@���@�\)@|I�@q��@h��@`�9@V�@O�P@G�@BM�@;�
@7\)@0�u@+�@'�P@"�@{@��@/@  @dZ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�B�B�B�B�B�B��BhB�B�wBÖBĜB��B��B��B��B�B�
B��B�5B�B�B��B  BBDBbB"�B/B2-B2-B+B�B �B'�B.B1'B+B �B�B�B�B!�B!�B�B�B�B�BhBPB1BBBB��B�B�fB�;B�/B�
B��B�?B�B��B��B�oB�bB�JB� BcTBE�B2-B:^B33B$�BJB��B�sB�B��B��B��B�B��B�oB�DB�1B�%B�By�Bo�BaHBO�B:^B)�B#�B�BbB
��B
�B
�B
�sB
�`B
�BB
�B
��B
��B
��B
��B
ǮB
B
�dB
�3B
��B
��B
��B
�JB
�B
p�B
e`B
ZB
G�B
9XB
2-B
(�B
"�B
�B
VB
1B	��B	��B	�BB	�qB	�B	��B	��B	��B	�VB	�B	v�B	p�B	gmB	^5B	VB	M�B	B�B	?}B	<jB	6FB	,B	!�B	�B	�B	hB	1B	B��B�B�B�B�`B�NB�;B�`B�TB�B��BĜB�wB�XB�9B�B�B�B�B�B�B��B��B��B��B��B��B�{B�hB�\B�PB�DB�=B�+B�B�B�B� B~�B�B�B�B�B�B�B� B}�B|�By�Bw�Bu�Bs�Bq�Bo�Bk�BiyBhsBe`BbNB`BB^5B]/B]/B^5B]/B\)B_;B_;BbNBbNBbNBbNBbNBaHBbNBffBiyBn�Bn�Bl�Bp�Bo�Bo�Bo�Bo�Bq�Bu�Bt�Bs�Bs�Bt�Bz�B{�B{�Bz�B{�B}�B�B�B�B�B�+B�7B�DB�JB�VB��B��B��B��B�B�'B�-B�3B�9B�?B�RB�dB�jB�jB�qB�wBĜBƨBƨBȴB��B��B��B��B�B�/B�HB�TB�ZB�`B�sB�B�B�B�B�B�B��B��B��B��B��B	B	+B		7B	JB	VB	\B	bB	oB	oB	uB	�B	�B	�B	�B	�B	�B	"�B	%�B	-B	/B	/B	1'B	5?B	;dB	>wB	A�B	B�B	E�B	J�B	L�B	N�B	N�B	Q�B	T�B	VB	YB	^5B	^5B	_;B	`BB	iyB	p�B	q�B	r�B	t�B	y�B	|�B	� B	�B	�B	�%B	�1B	�DB	�PB	�\B	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�'B	�-B	�3B	�?B	�FB	�LB	�LB	�XB	�qB	�wB	�wB	�}B	�}B	�}B	�}B	��B	B	ÖB	ĜB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�#B	�B	�)B	�5B	�5B	�5B	�5B	�5B	�5B	�;B	�NB	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B	��B
  B
  B
B
B
B
B
B
B
B
%B
+B
+B
+B
+B
+B
+B
+B
1B
1B
1B
1B
	7B
	7B

=B
DB
DB
DB
DB
DB

=B
	7B
	7B

=B
�B
"�B
,B
2-B
8RB
9XB
>wB
E�B
K�B
P�B
YB
^5B
aHB
e`B
iyB
l�B
o�B
t�B
y�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�B�B�B�B�B�B�B�B��BMB��B�dB�~BŊB˪BλB��B��B��B��B˯B�B�B��B��B �B�B3BOB#�B0	B3B3B+�B�B!�B(�B/B2B+�B!�B�B�B �B"�B"�B �B�B�BwBVB:B	#BBB�B��B�B�QB�$B�B��B�wB�,B�B��B��B�_B�NB�5B��Bd?BF�B3B;JB4B%�B4B��B�_B��B��B��B˪B��B��B�[B�-B�B�B��Bz�Bp�Bb9BP�B;NB*�B$�B�BIB
��B
�B
�vB
�XB
�KB
�)B
��B
μB
̳B
˨B
˯B
ȘB
�xB
�NB
�B
��B
��B
�iB
�2B
��B
q�B
fKB
[B
H�B
:AB
3B
)�B
#�B
�B
=B
	B	��B	��B	�)B	�ZB	��B	��B	��B	��B	�=B	��B	w�B	q�B	hUB	_B	V�B	N�B	CvB	@dB	=QB	7.B	,�B	"�B	�B	qB	LB		B	�B��B��B��B�gB�FB�4B�!B�HB�>B�BηBŁB�]B�;B�B��B��B��B��B��B��B��B��B��B��B�yB�nB�_B�OB�CB�6B�+B�$B�B�B��B��B��B�B��B�B��B�B��B��B��B~�B}�Bz�Bx�Bv�Bt�Br�Bp�BlnBj\BiUBfFBc5Ba)B_B^B^B_B^B]B`#B`!Bc2Bc2Bc4Bc5Bc5Bb-Bc5BgOBj^Bo}Bo~BmpBq�Bp�Bp�Bp�Bp�Br�Bv�Bu�Bt�Bt�Bu�B{�B|�B|�B{�B|�B~�B��B�B�B��B�B� B�*B�1B�<B�fB��B��B��B��B�B�B�B�B�$B�9B�KB�TB�MB�[B�[BńBǑBǎBəB˦BμB��B��B��B�B�3B�;B�BB�DB�ZB�mB�sB�B�B�B��B��B��B��B��B��B	B	B	
B	3B	@B	DB	HB	XB	XB	[B	iB	lB	oB	�B	 �B	 �B	#�B	&�B	-�B	0B	0B	2B	6&B	<MB	?_B	BqB	CyB	F�B	K�B	M�B	O�B	O�B	R�B	U�B	V�B	Y�B	_B	_B	`!B	a*B	jaB	q�B	r�B	s�B	u�B	z�B	}�B	��B	��B	� B	�B	�B	�/B	�7B	�GB	�MB	�SB	�nB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�B	�B	�B	�B	�'B	�0B	�4B	�7B	�@B	�]B	�_B	�^B	�eB	�hB	�gB	�fB	�lB	�wB	ĀB	ńB	ȔB	ɚB	ʢB	˭B	̯B	̰B	ͶB	κB	ξB	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	� B	�!B	�B	�&B	�6B	�NB	�XB	�YB	�`B	�dB	�nB	�pB	�|B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
�B	��B
 �B
 �B
�B
�B
�B
�B
B
B
B
B
B
B
B
B
B
B
B
	B
	B
	B
	B

"B

B
$B
/B
0B
.B
.B
.B
)B

B

 B
*B
pB
#�B
,�B
3B
9?B
:@B
?dB
F�B
L�B
Q�B
Z B
_B
b5B
fJB
jaB
mwB
p�B
u�B
z�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.12 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =1(+/-0), vertically averaged dS =0.001(+/-0.001) in PSS-78.                                                                                                                                                                                                  Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Map scales: x=6,3; y=2,1. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                            201906040939592019060409395920190604093959  AO  ARCAADJP                                                                    20160214031522    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160214031522  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160214031522  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604093959  IP                  G�O�G�O�G�O�                