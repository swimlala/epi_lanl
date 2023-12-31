CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:09:11Z creation      
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
_FillValue                    �`Argo profile    3.1 1.2 19500101000000  20200619170911  20220204114418  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               MA   AO  7662                            2C  D   APEX                            8312                            080318                          846 @ؾ^(d.�1   @ؾ^�o�@77
=p���c�C��%1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    MA   B   B   @���@�  A   AffA@  A`  A�  A�  A���A���A�33A�  A�  A�  B   B  BffB  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C�C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&�C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX�CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
y�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� D@��DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DPfDP� DQ  DQ� DRfDR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dy�=D��D�Y�D��\D��D��D�W�D��{D��RD��D�[�D�w�D��
D��D�VDچD��3D�HD�L�D�RD�{�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�@�(�@�(�Az�A>{A^{A~{A�
=A��
A���A�=pA�
=A�
=A�
=A�
=B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C�HC��C�HC�HC	�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC!�HC#�HC%��C'�HC)�HC+�HC-�HC/�HC1�HC3�HC5�HC7�HC9�HC;�HC=�HC?�HCA�HCC�HCE�HCG�HCI�HCK�HCM�HCO�HCQ�HCS�HCU�HCW��CY�HC[�HC]�HC_�HCa�HCc�HCe�HCg�HCi�HCk�HCm�HCo�HCq�HCs�HCu�HCw�HCy�HC{�HC}�HC�HC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��qC��C��C��C��C��C��C��qC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��qC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��qC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��qC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C���C��C��C��D xRD �RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RD	xRD	�RD
q�D
�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RD xRD �RD!xRD!�RD"xRD"�RD#xRD#�RD$xRD$�RD%xRD%�RD&xRD&�RD'xRD'�RD(xRD(�RD)xRD)�RD*xRD*�RD+xRD+�RD,xRD,�RD-xRD-�RD.xRD.�RD/xRD/�RD0xRD0�RD1xRD1�RD2xRD2�RD3xRD3�RD4xRD4�RD5xRD5�RD6xRD6�RD7xRD7�RD8xRD8�RD9xRD9�RD:xRD:�RD;xRD;�RD<xRD<�RD=xRD=�RD>xRD>�RD?xRD?�RD@xRD@��DAxRDA�RDBxRDB�RDCxRDC�RDDxRDD�RDExRDE�RDFxRDF�RDGxRDG�RDHxRDH�RDIxRDI�RDJxRDJ�RDKxRDK�RDLxRDL�RDMxRDM�RDNxRDN�RDOxRDO��DPxRDP�RDQxRDQ��DRxRDR�RDSxRDS�RDTxRDT�RDUxRDU�RDVxRDV�RDWxRDW�RDXxRDX�RDYxRDY�RDZxRDZ�RD[xRD[�RD\xRD\�RD]xRD]�RD^xRD^�RD_xRD_�RD`xRD`�RDaxRDa�RDbxRDb�RDcxRDc�RDdxRDd�RDexRDe�RDfxRDf�RDgxRDg�RDhxRDh�RDixRDi�RDjxRDj�RDkxRDk�RDlxRDl�RDmxRDm�RDnxRDn�RDoxRDo�RDpxRDp�RDqxRDq�RDrxRDr�RDsxRDs�RDtq�Dy��D��D�VD���D��HD��D�S�D���D��{D�
D�X D�s�D��3D��D�R=Dڂ=D��\D�qD�H�D�{D�w�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�/A�/A�1'A�/A�(�A�&�A��A��A�+A�"�A��Aɟ�Aȩ�A��/A���AÝ�A©�A�  A��uA���A���A��FA�hsA���A�-A��/A��A�ZA���A�+A�VA���A�r�A���A��A��RA�bA��
A��HA��mA���A�%A�bNA���A��yA���A���A���A�G�A�ffA��^A�^5A�oA��PA��A���A���A�I�A��FA�l�A�1'A��A�%A���A���A�bNA�7LA�{A�ȴA�%A��+A�VA�/A��A��uA�l�A�Q�A�;dA�I�A���A�-A��A��A���A�5?A�r�A��
A���A��A���A�x�A���A��
A�S�A�r�A�`BA�ƨA��A���A��A�VA�jA���A���A��HA���A�C�A��TA���A�hsA7LAz�+Au��AsXAq+Am��Aj��Ah{Afz�Ad�`Abz�A`��A_��A]��A[�AZz�AY�
AY&�AW�^AV��AUS�ATA�AS��AR5?AQ�AP��AO��ANr�AN{AM�
AM7LAL^5AK`BAJ~�AH��AF��AE&�AD$�ABȴAA�mA@�yA?��A>��A>VA=��A<�A9��A8{A6�A6�A5K�A4�!A2��A1A1"�A0~�A0=qA0A/`BA.�A-�A,�A,VA+�mA)�7A't�A&�/A&�HA&~�A&JA$��A$(�A#%A"�!A"M�A ��A�hA��A�^A�jA9XA�
A�A`BA��A�DA�FA�`A1'A�A{A�HA�An�A7LA��A;dA9XAȴA$�AO�A	��A�jAK�A�9A�A-A�7A�HAZA�7A�/A{A�A��A��A/@�\)@�%@�ƨ@��`@�@��u@�I�@�@�C�@�!@�@��@�7L@�bN@�l�@�/@◍@��@�1'@���@�V@�5?@ݡ�@��@�r�@�
=@�$�@�@؛�@��@�b@ӝ�@�"�@с@϶F@Η�@�^5@���@Η�@�dZ@�"�@ΰ!@Ώ\@�E�@���@�%@�1@�|�@ʧ�@Ɂ@��@�9X@ǍP@�t�@ǍP@�dZ@��@���@ư!@�$�@ċD@�1'@Å@�"�@�ȴ@��T@�j@��F@���@�=q@�J@�@���@��@�/@�z�@�I�@���@�"�@��y@��!@���@�-@���@�hs@�/@�Ĝ@��@�j@�b@�t�@��!@���@�hs@�7L@���@���@�(�@�|�@���@��@��@�5?@���@�x�@�r�@��@�dZ@�@���@�$�@�n�@�$�@���@��@�&�@���@�bN@�9X@�S�@���@���@���@���@�I�@�1@�C�@��u@�1@��P@��y@���@��@��@�%@�r�@�1@�  @�  @�  @��
@�\)@��@��@�n�@���@�O�@���@�z�@��
@��P@�;d@�ȴ@�ff@�M�@���@���@��-@�`B@��9@��D@�z�@�bN@�Q�@�1@���@�dZ@��@��@�"�@��@�
=@��y@���@��\@�^5@�J@���@��h@�G�@�/@��@���@�r�@�bN@�9X@��
@��P@�;d@���@�^5@�J@�@��h@��@��/@��u@�I�@�1'@�1'@��;@�\)@�33@�
=@��@�ȴ@�v�@�^5@�V@�=q@�{@�@��@��^@���@�?}@���@��u@�r�@��D@�Z@���@���@�\)@�33@�@�ȴ@���@�n�@�$�@��@��@�@�@�X@��`@��@�r�@�A�@���@���@�K�@�33@�
=@��H@��H@��@��!@��\@�ff@�=q@��@�p�@���@��u@�1'@�1@��;@���@��P@�S�@��@|y>@s��@i�@bں@[C�@R�y@J��@D_@>�M@8�@333@-w2@)}�@$��@ N�@��@�@��@�@	��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A�/A�/A�1'A�/A�(�A�&�A��A��A�+A�"�A��Aɟ�Aȩ�A��/A���AÝ�A©�A�  A��uA���A���A��FA�hsA���A�-A��/A��A�ZA���A�+A�VA���A�r�A���A��A��RA�bA��
A��HA��mA���A�%A�bNA���A��yA���A���A���A�G�A�ffA��^A�^5A�oA��PA��A���A���A�I�A��FA�l�A�1'A��A�%A���A���A�bNA�7LA�{A�ȴA�%A��+A�VA�/A��A��uA�l�A�Q�A�;dA�I�A���A�-A��A��A���A�5?A�r�A��
A���A��A���A�x�A���A��
A�S�A�r�A�`BA�ƨA��A���A��A�VA�jA���A���A��HA���A�C�A��TA���A�hsA7LAz�+Au��AsXAq+Am��Aj��Ah{Afz�Ad�`Abz�A`��A_��A]��A[�AZz�AY�
AY&�AW�^AV��AUS�ATA�AS��AR5?AQ�AP��AO��ANr�AN{AM�
AM7LAL^5AK`BAJ~�AH��AF��AE&�AD$�ABȴAA�mA@�yA?��A>��A>VA=��A<�A9��A8{A6�A6�A5K�A4�!A2��A1A1"�A0~�A0=qA0A/`BA.�A-�A,�A,VA+�mA)�7A't�A&�/A&�HA&~�A&JA$��A$(�A#%A"�!A"M�A ��A�hA��A�^A�jA9XA�
A�A`BA��A�DA�FA�`A1'A�A{A�HA�An�A7LA��A;dA9XAȴA$�AO�A	��A�jAK�A�9A�A-A�7A�HAZA�7A�/A{A�A��A��A/@�\)@�%@�ƨ@��`@�@��u@�I�@�@�C�@�!@�@��@�7L@�bN@�l�@�/@◍@��@�1'@���@�V@�5?@ݡ�@��@�r�@�
=@�$�@�@؛�@��@�b@ӝ�@�"�@с@϶F@Η�@�^5@���@Η�@�dZ@�"�@ΰ!@Ώ\@�E�@���@�%@�1@�|�@ʧ�@Ɂ@��@�9X@ǍP@�t�@ǍP@�dZ@��@���@ư!@�$�@ċD@�1'@Å@�"�@�ȴ@��T@�j@��F@���@�=q@�J@�@���@��@�/@�z�@�I�@���@�"�@��y@��!@���@�-@���@�hs@�/@�Ĝ@��@�j@�b@�t�@��!@���@�hs@�7L@���@���@�(�@�|�@���@��@��@�5?@���@�x�@�r�@��@�dZ@�@���@�$�@�n�@�$�@���@��@�&�@���@�bN@�9X@�S�@���@���@���@���@�I�@�1@�C�@��u@�1@��P@��y@���@��@��@�%@�r�@�1@�  @�  @�  @��
@�\)@��@��@�n�@���@�O�@���@�z�@��
@��P@�;d@�ȴ@�ff@�M�@���@���@��-@�`B@��9@��D@�z�@�bN@�Q�@�1@���@�dZ@��@��@�"�@��@�
=@��y@���@��\@�^5@�J@���@��h@�G�@�/@��@���@�r�@�bN@�9X@��
@��P@�;d@���@�^5@�J@�@��h@��@��/@��u@�I�@�1'@�1'@��;@�\)@�33@�
=@��@�ȴ@�v�@�^5@�V@�=q@�{@�@��@��^@���@�?}@���@��u@�r�@��D@�Z@���@���@�\)@�33@�@�ȴ@���@�n�@�$�@��@��@�@�@�X@��`@��@�r�@�A�@���@���@�K�@�33@�
=@��H@��H@��@��!@��\@�ff@�=q@��@�p�@���@��u@�1'@�1@��;@���@��PG�O�@��@|y>@s��@i�@bں@[C�@R�y@J��@D_@>�M@8�@333@-w2@)}�@$��@ N�@��@�@��@�@	��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
�fB
�fB
�fB
�fB
�fB
�fB
�mB
�mB
�fB
�fB
�mB
�B
��B49BiyB�oB��B��B��B�dB�jB�}B��B��B��B��BǮB�#B�TB�`B�fB�yB�B��B��BBJB{B!�B6FB;dBK�BffB�B��B��B��B��B��B�B�B�B�B�?B�9B�!B�B�-B�3B�?B�XB�qB�}BĜB��B��B��B��B��B��B��B��B��B��B��B��BɺBƨB�?B�1Bz�B}�B�=B�Bx�Be`B8RB�B��B�B�^B�oB� Bw�Bn�BW
B7LB{B	7B+BB
��B
�ZB
�#B
ÖB
�B
��B
�oB
�oB
�bB
u�B
\)B
>wB
0!B
#�B
PB	��B	�B	�;B	�B	��B	�qB	�?B	�B	��B	��B	��B	�oB	�=B	�B	}�B	w�B	t�B	m�B	iyB	e`B	`BB	ZB	W
B	T�B	R�B	L�B	G�B	@�B	7LB	,B	"�B	�B	�B	hB	VB	B	B��B��B�B�HB�B��B��BƨBĜB�wB�RB�LB�?B�3B�-B�!B�B�B��B��B��B��B��B��B��B��B��B�hB�\B�PB�=B�+B�B~�B{�Bx�Bu�Bt�Br�Br�Bq�Bp�Bm�BjBffBe`BcTB`BB^5B[#B[#BVBVBS�BR�BR�BO�BP�BM�BK�BK�BH�BH�BG�BG�BF�BE�BD�BD�BD�BC�BC�BB�BB�BA�B?}B?}B>wB=qB=qB=qB=qB<jB;dB<jB>wB=qB<jB<jB<jB>wB>wB>wBA�BA�BA�BB�BB�BB�BB�BB�BA�BB�BG�BH�BK�BQ�B[#BZB_;B_;BgmBr�B~�B�B�+B�VB�hB�uB��B��B��B��B��B��B��B��B�B�'B�-B�-B�3B�?B�FB�LB�^B�dB�dB�jB�wB��BÖBɺB��B��B��B��B��B�B�B�#B�;B�TB�`B�mB�sB�B�B�B��B��B��B��B��B	B	+B	
=B	PB	PB	PB	VB	\B	hB	oB	{B	�B	�B	�B	"�B	%�B	&�B	(�B	+B	-B	/B	2-B	5?B	6FB	6FB	7LB	7LB	7LB	7LB	5?B	49B	6FB	8RB	:^B	;dB	;dB	;dB	7LB	7LB	7LB	5?B	8RB	<jB	<jB	<jB	<jB	>wB	>wB	>wB	?}B	A�B	C�B	E�B	G�B	H�B	J�B	K�B	L�B	N�B	P�B	Q�B	S�B	XB	[#B	\)B	^5B	`BB	aHB	cTB	gmB	hsB	iyB	jB	jB	m�B	q�B	s�B	v�B	x�B	z�B	}�B	� B	�B	�B	�B	�B	�+B	�7B	�=B	�JB	�PB	�PB	�bB	�hB	�oB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�3B	�?B	�LB	�RB	�XB	�^B	�dB	�qB	��B	�}B	�}B	�}B	��B	�}B	�}B	ÖB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�/B	�5B	�;B	�BB	�HB	�HB	�NB	�NB	�ZB	�`B	��B
�B
�B
	B
$�B
-�B
6B
=<B
B�B
G�B
MjB
T,B
W�B
Z�B
_!B
dZB
f�B
ncB
poB
w211111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�OB,�Ba�B��B�5B�)B�#B��B��B��B�)B�BB�BB�$B�B�yB۪BݶB޼B��B��B�B�<B�[B�B�BB.�B3�BDB^�B{`B�B�)B�)B�)B�BB�TB�NB�NB�`B��B��B�mB�gB�yB�B��B��B��B��B��B�0B�BB�BB�=B�7B�B�0B�IB�CB�0B�$B�B�B��B��B��Bs2BvEB��B{cBq&B]�B0�B�B�5B�`B��B��BxaBp1Bf�BOnB/�B�B�B
��B
�|B
�LB
��B
ӐB
�B
�~B
�B
��B
��B
��B
n7B
T�B
6�B
(�B
QB
�B	�XB	��B	׺B	΄B	�BB	��B	��B	��B	�OB	�B	�B	��B	��B	}�B	v{B	pVB	mCB	fB	bB	]�B	X�B	R�B	O�B	M�B	K|B	EWB	@9B	9B	/�B	$�B	_B	LB	!B		�B	�B��B��B�~B�`B�5B��BУB�yB�aB�<B�1B�B��B��B��B��B��B��B��B��B�zB�nB��B�VB�&B� B�&B� B�B�B��B��B��B�B}�Bw�Bt�BqpBn_BmXBkLBkLBjFBi@Bf-BcB_B]�B[�BX�BV�BS�BS�BN�BN�BL�BK�BK�BHBI�BFsBDgBDhBAUBAUB@OB@OB?IB>CB==B=>B=>B<8B<8B;1B;1B:+B8B8B7B6B6B6B6B5B4B5B7B6B5B5B5B7B7B7B:-B:-B:-B;3B;3B;3B;3B;3B:-B;3B@RBAXBDkBJ�BS�BR�BW�BW�B`BkRBw�B{�B�B��B�	B�B�-B�^B�qB�qB�kB�^B�eB��B��B��B��B��B��B��B��B��B��B�B�B�	B�B�"B�5B�XB�qB�wBʊBːB̖B΢BѵB��B��B��B��B�
B�B�.B�;B�RB�_B�qB�B��B��B��B��B	�B	�B	�B	�B	�B	�B	
B	
B	B	B	B	MB	kB	}B	�B	!�B	#�B	%�B	'�B	*�B	-�B	.�B	.�B	/�B	/�B	/�B	/�B	-�B	,�B	.�B	0�B	2�B	3�B	3�B	3�B	/�B	/�B	/�B	-�B	0�B	5B	5B	5B	5B	7B	7B	7B	8B	:#B	<0B	>;B	@GB	AMB	CZB	D`B	EfB	GrB	I~B	J�B	L�B	P�B	S�B	T�B	V�B	X�B	Y�B	[�B	`B	aB	bB	cB	cB	f)B	jAB	lMB	o`B	qlB	sxB	v�B	x�B	y�B	{�B	|�B	}�B	�B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�)B	�5B	�AB	�GB	�MB	�ZB	�fB	�xB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�*B	�;B	�GB	�TB	�ZB	�ZB	�`B	�ZB	�`B	�`B	�`B	�fB	�fB	�fB	�lB	�rB	˅B	̋B	̋B	̋B	͑B	ΗB	ϝB	УB	УB	УB	УB	ѪB	УB	УB	ѪB	ҰB	ӶB	��B	��B	��B	��B	��B	��B	��B	��G�O�B	��B	�zB	�GB
�B
�B
�B
&<B
.�B
5�B
;B
@WB
E�B
L�B
PPB
S|B
W�B
\�B
_CB
f�B
h�B
o�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.12 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.007(+/-0.003) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144182022020411441820220204114418  AO  ARCAADJP                                                                    20200619170911    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170911  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170911  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114418  IP                  G�O�G�O�G�O�                