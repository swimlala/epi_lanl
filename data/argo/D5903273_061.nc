CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2019-02-19T18:16:48Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20190219181648  20200831164645  5903273 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               =A   AO  3334                            2C  D   APEX                            4917                            041310                          846 @��Z�C�~1   @��[K� �@6�ě��T�b�     1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    =A   B   B   @333@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A���A�  B   B  B  B  B   B(  B0  B8  B@ffBH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C=�fC@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dpy�Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dy�=D�)D�T)D��RD���D��D�A�D�mD��RD��fD�S3D���D���D���D�H�D�y�D��D�D�3�D�a�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @;�@�(�@�(�A{A"{AB{Ab{A�
=A�
=A�
=A�
=A�
=A�
=A��
A�
=B �B�B�B�B �B(�B0�B8�B@�BH�BP�BX�B`�Bh�Bp�Bx�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C !HC!HC!HC!HC!HC
!HC!HC!HC!HC!HC!HC!HC!HC!HC!HC!HC !HC"!HC$!HC&!HC(!HC*!HC,!HC.!HC0!HC2!HC4!HC6!HC8!HC:!HC<!HC>�C@!HCB!HCD!HCF!HCH!HCJ!HCL!HCN!HCP!HCR!HCT!HCV!HCX!HCZ!HC\!HC^!HC`!HCb!HCd!HCf!HCh!HCj!HCl!HCn!HCp!HCr!HCt!HCv!HCx!HCz!HC|!HC~!HC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D RD �RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RD	RD	�RD
RD
�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RD RD �RD!RD!�RD"RD"�RD#RD#�RD$RD$�RD%RD%�RD&RD&�RD'RD'�RD(RD(�RD)RD)�RD*RD*�RD+RD+�RD,RD,�RD-RD-�RD.RD.�RD/RD/�RD0RD0�RD1RD1�RD2RD2�RD3RD3�RD4RD4�RD5RD5�RD6RD6�RD7RD7�RD8RD8�RD9RD9�RD:RD:�RD;RD;�RD<RD<�RD=RD=�RD>RD>�RD?RD?�RD@RD@�RDARDA�RDBRDB�RDCRDC�RDDRDD�RDERDE�RDFRDF�RDGRDG�RDHRDH�RDIRDI�RDJRDJ�RDKRDK�RDLRDL�RDMRDM�RDNRDN�RDORDO�RDPRDP�RDQRDQ�RDRRDR�RDSRDS�RDTRDT�RDURDU�RDVRDV�RDWRDW�RDXRDX�RDYRDY�RDZRDZ�RD[RD[�RD\RD\�RD]RD]�RD^RD^�RD_RD_�RD`RD`�RDaRDa�RDbRDb�RDcRDc�RDdRDd�RDeRDe�RDfRDf�RDgRDg�RDhRDh�RDiRDi�RDjRDj�RDkRDk�RDlRDl�RDmRDm�RDnRDn�RDoRDo�RDpRDp��DqRDq�RDrRDr�RDsRDs�RDtRDt�RDy��D�RD�XRD��{D��
D��D�FD�qHD��{D���D�W\D��D���D���D�L�D�}�D��D�
=D�8 D�fD���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A��A�A�%A�%A�%A�%A�A�A�
=A�VA�bA�bA�bA�bA�oA�oA�{A�{A�{A�{A��A��A�{A��A��A��A��A��A��A��A�{A�1A���A�v�A��A��!A�ffA�/A��A�(�A�A�A��7A�C�A� �A��TA���A�G�A���A� �A��A�?}A���A�r�A�A�A��-A���A�~�A�bNA�K�A�1'A���A��FA�+A�^5A�;dA��\A��A�`BA�7LA��A�ZA���A�Q�A�7LA�C�A�~�A��/A�1A��jA�ĜA���A�/A���A�A��;A��jA��HA���A�|�A���A�ZA�bA��^A�%A���A�  A���A��A�
=A�O�A�&�A���A���A�bNA�z�A��+A�/A��hA�VA��!A�hsA��A��/A��-A�|�A��^A���A���A�p�A�Q�A��mA���A��TA�O�Al�A|��AzM�Av��Ar �AnjAl5?AjI�AfE�AcƨAa��A_�A^VA\r�AZ1'AW�^AT��AR��AQ
=AO��AO7LAN1'AM�#ALv�AK��AH�DAF{AE�mAES�AC;dA@��A@-A?VA=XA9�A8�HA8�A8bNA7�#A6�yA5ƨA4��A21A1�PA0M�A/&�A-dZA+�
A*��A)p�A(�HA(�A(  A&�A%K�A#�wA"�RA!�A!oA ZA��A�A��A��A&�A��A��A�AS�A"�A��A�AbAG�A  A��An�An�AbNA9XA�TA&�A�A�FA�+A��A�9A��A/A	�TA��AM�A1Al�A�TA;dAȴAbNAO�A�uA  A�7A/A �\@���@�E�@���@�9X@�;d@��D@���@�@�$�@�7L@�j@��;@�
=@�n�@��@�hs@�bN@�\)@���@�ff@�"�@�%@ߕ�@��H@��T@���@��@��@� �@�M�@��
@�5?@��#@�r�@�^5@���@�t�@�$�@���@�@�@���@��w@�+@��\@��-@�?}@�A�@��@��R@���@�5?@�$�@���@�p�@�bN@��
@�S�@��H@��-@�G�@��/@���@��-@���@��\@��-@��u@��!@���@�ff@�=q@��T@�n�@�ȴ@�X@�b@���@�K�@�{@�@�@���@���@�O�@��u@�r�@��@�%@�hs@�O�@��@�Q�@��;@���@��9@��@�K�@�K�@��P@�9X@���@���@���@�&�@���@���@�"�@�o@��@��7@�p�@��7@�?}@�%@���@���@��@���@���@���@��@�?}@�x�@�?}@��@��@�G�@�x�@��@��F@��P@�C�@�l�@�\)@�33@�+@���@���@�p�@�&�@���@��/@���@�Q�@�Z@��@�X@�G�@�G�@���@���@��7@�/@��/@���@� �@�+@��R@���@��@�E�@��#@��T@�x�@��@��@�bN@��@��@�dZ@�
=@��@��!@���@��^@�`B@�%@��D@�z�@�bN@�b@��w@�K�@��H@��\@�^5@�@��^@�hs@�&�@��@��j@��D@�Z@�1'@���@���@���@�dZ@�C�@�;d@�
=@���@�E�@��@��-@�7L@��@��j@��j@��u@�I�@� �@��P@�\)@�"�@��y@���@�v�@�ff@�{@�@�hs@�G�@��@�%@���@��@�z�@�9X@��F@���@�l�@�C�@�;d@�;d@�33@��@�@��@��R@���@�~�@�^5@�@��h@��@��@�x�@�O�@�?}@�&�@��/@��9@��@�I�@�(�@���@���@�U2@|�@t�5@k�W@b�@Y<6@RR�@K�@F�@@�4@;�@3˒@.�@)@$V�@!�"@��@�@�5@�?@�X111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A��A��A�A�%A�%A�%A�%A�A�A�
=A�VA�bA�bA�bA�bA�oA�oA�{A�{A�{A�{A��A��A�{A��A��A��A��A��A��A��A�{A�1A���A�v�A��A��!A�ffA�/A��A�(�A�A�A��7A�C�A� �A��TA���A�G�A���A� �A��A�?}A���A�r�A�A�A��-A���A�~�A�bNA�K�A�1'A���A��FA�+A�^5A�;dA��\A��A�`BA�7LA��A�ZA���A�Q�A�7LA�C�A�~�A��/A�1A��jA�ĜA���A�/A���A�A��;A��jA��HA���A�|�A���A�ZA�bA��^A�%A���A�  A���A��A�
=A�O�A�&�A���A���A�bNA�z�A��+A�/A��hA�VA��!A�hsA��A��/A��-A�|�A��^A���A���A�p�A�Q�A��mA���A��TA�O�Al�A|��AzM�Av��Ar �AnjAl5?AjI�AfE�AcƨAa��A_�A^VA\r�AZ1'AW�^AT��AR��AQ
=AO��AO7LAN1'AM�#ALv�AK��AH�DAF{AE�mAES�AC;dA@��A@-A?VA=XA9�A8�HA8�A8bNA7�#A6�yA5ƨA4��A21A1�PA0M�A/&�A-dZA+�
A*��A)p�A(�HA(�A(  A&�A%K�A#�wA"�RA!�A!oA ZA��A�A��A��A&�A��A��A�AS�A"�A��A�AbAG�A  A��An�An�AbNA9XA�TA&�A�A�FA�+A��A�9A��A/A	�TA��AM�A1Al�A�TA;dAȴAbNAO�A�uA  A�7A/A �\@���@�E�@���@�9X@�;d@��D@���@�@�$�@�7L@�j@��;@�
=@�n�@��@�hs@�bN@�\)@���@�ff@�"�@�%@ߕ�@��H@��T@���@��@��@� �@�M�@��
@�5?@��#@�r�@�^5@���@�t�@�$�@���@�@�@���@��w@�+@��\@��-@�?}@�A�@��@��R@���@�5?@�$�@���@�p�@�bN@��
@�S�@��H@��-@�G�@��/@���@��-@���@��\@��-@��u@��!@���@�ff@�=q@��T@�n�@�ȴ@�X@�b@���@�K�@�{@�@�@���@���@�O�@��u@�r�@��@�%@�hs@�O�@��@�Q�@��;@���@��9@��@�K�@�K�@��P@�9X@���@���@���@�&�@���@���@�"�@�o@��@��7@�p�@��7@�?}@�%@���@���@��@���@���@���@��@�?}@�x�@�?}@��@��@�G�@�x�@��@��F@��P@�C�@�l�@�\)@�33@�+@���@���@�p�@�&�@���@��/@���@�Q�@�Z@��@�X@�G�@�G�@���@���@��7@�/@��/@���@� �@�+@��R@���@��@�E�@��#@��T@�x�@��@��@�bN@��@��@�dZ@�
=@��@��!@���@��^@�`B@�%@��D@�z�@�bN@�b@��w@�K�@��H@��\@�^5@�@��^@�hs@�&�@��@��j@��D@�Z@�1'@���@���@���@�dZ@�C�@�;d@�
=@���@�E�@��@��-@�7L@��@��j@��j@��u@�I�@� �@��P@�\)@�"�@��y@���@�v�@�ff@�{@�@�hs@�G�@��@�%@���@��@�z�@�9X@��F@���@�l�@�C�@�;d@�;d@�33@��@�@��@��R@���@�~�@�^5@�@��h@��@��@�x�@�O�@�?}@�&�@��/@��9@��@�I�@�(�@���G�O�@�U2@|�@t�5@k�W@b�@Y<6@RR�@K�@F�@@�4@;�@3˒@.�@)@$V�@!�"@��@�@�5@�?@�X111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BĜB��B�BG�BXBjBt�Bz�B�+B�hB�oB�oB�{B��B��B�B�B�B�B�B�!B�FB�RB�jBĜBȴBǮBƨBĜB�wB�RB�3B�'B�!B��B��B��B�7Bo�B[#BJ�B8RB�B�`B��B��B�DB�=Bq�Be`BP�BB�B<jB:^B;dBm�B}�B}�Bt�Bp�Bm�BffBYB7LB&�B �BB��B�B��B��B��BɺB�XB��B�\B�By�Br�Bm�BffB^5BD�B,B�B
�B
�/B
�B
�jB
�oB
~�B
l�B
^5B
D�B
.B
�B	��B	�B	ĜB	�?B	��B	�JB	{�B	m�B	`BB	XB	K�B	=qB	.B	�B	oB		7B	B	B	  B	B��B��B�B�B�yB�fB�BB�)B�B�B��BƨBÖB��B��B�}B�}B�^B�9B�B�B��B��B��B��B�uB�bB�VB�JB�7B�B~�By�Bv�Bu�Bu�Bt�Bq�Bn�Bk�BiyBe`BcTB_;B\)BZBZB\)BcTBcTB`BBYBM�BQ�BR�BQ�BQ�BO�BM�BJ�BH�BF�BC�BA�B?}B<jB9XB7LB5?B49B2-B0!B.B-B,B+B+B)�B(�B&�B%�B$�B#�B"�B!�B�B�B�B�B�B�B�B�B �B%�B#�B�B�B"�B/B,B-B,B(�B%�B �B�BoBhBbBbBbBhBhBbBhBhBoBoB�B�B�B�B�B�B�B�B �B#�B'�B-B/B7LB@�BF�BF�BD�BD�BD�BE�BE�BE�BM�B]/Be`Bo�Bt�Bv�Bu�Bv�By�Bz�Bz�B~�B�+B�JB�DB�JB�bB��B��B��B��B��B��B��B��B�B�B�LB��B��B�}B�wB�qB�dB�dB�jB��BƨB��B��B��B��B��B��B��B�#B�BB�NB�TB�TB�B�B�B��B��B��B��B	  B	B	B		7B	DB	hB	�B	�B	�B	�B	�B	%�B	%�B	&�B	)�B	/B	/B	.B	/B	2-B	1'B	1'B	33B	49B	49B	6FB	7LB	8RB	;dB	A�B	A�B	C�B	F�B	N�B	Q�B	S�B	VB	XB	ZB	ZB	[#B	]/B	aHB	bNB	gmB	k�B	m�B	o�B	s�B	u�B	w�B	z�B	|�B	~�B	� B	�B	� B	�B	�+B	�=B	�JB	�JB	�JB	�PB	�\B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�'B	�3B	�?B	�LB	�LB	�^B	�jB	�qB	�wB	�wB	�}B	��B	B	ŢB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�#B	�)B	�/B	�/B	�/B	�/B	�5B	�5B	�;B	�;B	�;B	�HB	�NB	�NB	�`B	�`B	�`B	�`B	�fB	�fB	�fB	�mB	�mB	�sB	�yB	�yB	�yB	�B	�B	��B
�B
B
�B
%B
-CB
5B
9�B
@�B
EB
K�B
R�B
VB
\xB
_�B
d&B
h�B
l�B
p�B
s�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B�|B�|B�yB�wB�wB�wB�wB�}B�B�wB�yB�wB�yB�}B�}B�B�}B�B�}B�}B�}B�}B�}B�}B�}B�}B�}B�B�B�}B�}B�}BĔB��B|BG�BXBjrBt�Bz�B�B�^B�bB�fB�pB�zB��B��B�B�B��B��B�B�9B�MB�\BĐBȫBǢBƛBēB�jB�HB�&B�B�B��B��B��B�+Bo�B[BJ�B8EB�B�XB��B�|B�8B�4Bq�BeSBP�BB�B<^B:RB;VBm�B}�B}�Bt�Bp�Bm�Bf[BYB7BB&�B �BB��B�B��B��B��BɯB�LB��B�OB�By�Br�Bm�BfXB^'BD�B+�B�B
�B
�"B
��B
�\B
�cB
~�B
lB
^)B
D�B
.	B
yB	��B	�B	ĎB	�0B	��B	�<B	{�B	m�B	`4B	XB	K�B	=aB	.B	�B	aB		'B	B	�B��B	 �B��B��B�B�tB�kB�XB�2B�B�B��B��BƘBÆB�{B�vB�nB�oB�PB�+B�B��B��B��B��B�~B�eB�QB�FB�<B�*B�B~�By�Bv�Bu�Bu�Bt�Bq�Bn�BkvBijBeSBcDB_,B\BZBZB\BcCBcEB`1BYBM�BQ�BR�BQ�BQ�BO�BM�BJ�BH�BF�BC�BAzB?nB<[B9JB7=B5/B4(B2B0B.B,�B+�B*�B*�B)�B(�B&�B%�B$�B#�B"�B!�B�B�B�B�B�B�B�B�B �B%�B#�B�B�B"�B/B+�B,�B+�B(�B%�B �B�B_BWBRBOBRBVBWBTBYBWB_B_BpBvB{B�B�B�B�B�B �B#�B'�B,�B/	B7<B@tBF�BF�BD�BD�BD�BE�BE�BE�BM�B]BeOBo�Bt�Bv�Bu�Bv�By�Bz�Bz�B~�B�B�:B�2B�:B�TB�oB��B��B��B��B��B��B��B��B�B�:B�rB�sB�lB�fB�^B�SB�SB�XB�sBƚB��B��B��B��B��B��B��B�B�1B�<B�CB�CB�lB�B�B��B��B��B��B��B	�B	B		%B	2B	UB	tB	�B	�B	�B	�B	%�B	%�B	&�B	)�B	/B	/B	.B	/	B	2B	1B	1B	3 B	4(B	4)B	67B	7:B	8CB	;TB	AwB	AwB	C�B	F�B	N�B	Q�B	S�B	U�B	X B	ZB	ZB	[B	]B	a5B	b>B	g\B	ktB	mB	o�B	s�B	u�B	w�B	z�B	|�B	~�B	�B	��B	�B	�B	�B	�+B	�;B	�=B	�9B	�>B	�LB	�_B	�wB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�-B	�;B	�<B	�MB	�XB	�`B	�fB	�dB	�kB	�{B	�~B	ŐB	ǞB	ȣB	ɪB	˵B	̽B	̽B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�B	�B	�B	�'B	�"B	�*B	�*B	�*B	�8B	�?B	�>B	�PB	�LB	�PB	�LB	�SB	�UB	�SB	�[B	�ZB	�dB	�gB	�gB	�hG�O�B	��B	��B
oB
B
�B
$�B
-3B
4�B
9{B
@�B
EB
K�B
RxB
U�B
\fB
_�B
dB
h�B
l�B
p�B
s�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.13 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             202008311646452020083116464520200831164645  AO  ARCAADJP                                                                    20190219181648    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20190219181648  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20190219181648  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20200831164645  IP                  G�O�G�O�G�O�                