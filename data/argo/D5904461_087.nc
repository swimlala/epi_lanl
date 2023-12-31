CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-12-05T03:16:12Z AOML 3.0 creation; 2016-08-07T21:36:41Z UW 3.1 conversion     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7    PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7X   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
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
_FillValue                    gx   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ix   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qp   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �`   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �X   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �(   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �,   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �0   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20151205031612  20160807143641  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               WA   AO  5286_8897_087                   2C  D   APEX                            6531                            072314                          846 @׃���1   @׃��[!L@3e�S����c$z�G�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    WA   B   B   @@  @�  @�  A   A   A>ffA`  A�  A�  A�  A���A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB�  B���B�  B�  B�  B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%�fD&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dyy�D��D�33D��fD���D��D�@ D�y�D���D�fD�<�D�|�D��fD�3D�P Dڃ3D��D�  D�FfD�D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @HQ�@�(�@�(�A{A"{A@z�Ab{A�
=A�
=A�
=A��
A�
=A�
=A�
=A�
=B �B�B�B�B �B(�B0�B8�B@�BH�BP�BX�B`�Bh�Bp�Bx�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B���B�B�B��)B�B�B�B�B�B�B�u�B�\B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C !HC!HC!HC!HC!HC
!HC!HC!HC!HC!HC!HC!HC!HC!HC!HC!HC !HC"!HC$!HC&!HC(!HC*!HC,!HC.!HC0!HC2!HC4!HC6!HC8!HC:!HC<!HC>!HC@!HCB!HCD!HCF!HCH!HCJ!HCL!HCN!HCP!HCR!HCT!HCV!HCX!HCZ!HC\!HC^!HC`!HCb!HCd!HCf!HCh!HCj!HCl!HCn!HCp!HCr!HCt!HCv!HCx!HCz!HC|!HC~!HC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�qC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�qC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D RD �RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RD	RD	�RD
RD
�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RD RD �RD!RD!�RD"RD"�RD#RD#�RD$RD$�RD%RD%��D&RD&�RD'RD'�RD(RD(�RD)RD)�RD*RD*�RD+RD+�RD,RD,�RD-RD-�RD.RD.�RD/RD/�RD0RD0�RD1RD1�RD2RD2�RD3RD3�RD4RD4�RD5RD5�RD6RD6�RD7RD7�RD8RD8�RD9RD9�RD:RD:�RD;RD;�RD<RD<�RD=RD=�RD>RD>�RD?RD?�RD@RD@�RDARDA�RDBRDB�RDCRDC�RDDRDD�RDERDE�RDFRDF�RDGRDG�RDHRDH�RDIRDI�RDJRDJ�RDKRDK�RDLRDL�RDMRDM�RDNRDN�RDORDO�RDPRDP�RDQRDQ�RDRRDR�RDSRDS�RDTRDT�RDURDU�RDVRDV�RDWRDW�RDXRDX�RDYRDY�RDZRDZ�RD[RD[�RD\RD\�RD]RD]�RD^RD^�RD_RD_�RD`RD`�RDaRDa�RDbRDb�RDcRDc�RDdRDd�RDeRDe�RDfRDf�RDgRDg�RDhRDh�RDiRDi�RDjRDj�RDkRDk�RDlRDl�RDmRDm�RDnRDn�RDoRDo�RDpRDp�RDqRDq�RDrRDr�RDsRDs�RDtRDt��Dy��D��D�7\D���D���D��D�D)D�}�D���D��D�@�D���D�ڏD�\D�T)Dڇ\D���D�$)D�J�D��D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A�bA�oA�VA�bA�JA�JA�
=A�VA�VA�
=A�
=A�
=A�
=A�
=A�JA�JA�JA�VA�VA�bA�VA�VA�bA�bA�bA�oA�oA�{A�{A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A���A��`A�VA�dZA�?}A�S�A�K�AœuA��mAA��A��\A���A���A�1A���A��wA��A�bNA�5?A��A��\A�ƨA�C�A���A�~�A�33A���A�ĜA��jA��^A�`BA��A�33A��A��A��`A��A��+A��A��A���A�XA��jA��+A��wA�oA���A�A�A��A��+A�-A�G�A�ĜA��A�A��#A�n�A��A��A���A��PA��A���A}S�A{"�Ax��Aw"�Au�PAq�
An5?Al1'Aj5?Ah�/AgXAa
=A]�AY��AVz�AR�AP(�AN^5AM&�ALn�AK�wAJ�HAH�yAG/AFA�AEt�AC�;A?\)A>A�A=��A;l�A:^5A:  A9p�A9�A8��A8(�A6E�A5C�A3"�A2I�A2bNA1��A0^5A/ƨA/&�A-+A+K�A)?}A(ZA(1'A'"�A&�A$��A#�A"�yA!��A!�A ��A �A v�AK�A��A�A/AoA�/AM�AoAO�A�uA��A��AƨA;dA  A(�Ap�AXA��A&�A
�yA
�AO�A�wA�A�hA�#AVAz�A��A�/A��AbNA�A��A�hA�Ap�A�`AQ�A��AK�A �+@��H@�;d@�@�ff@��@���@��@��9@�?}@��@���@�R@旍@���@���@�@�P@�M�@Ӆ@�ff@У�@�@��m@���@��`@�b@�1@ӍP@ҟ�@�x�@�j@�(�@�t�@�;d@�\)@�j@��y@�Q�@�hs@� �@���@؋D@ו�@�-@�`B@�Ĝ@ԣ�@�j@��m@�o@�V@�j@ύP@Χ�@Ώ\@Ώ\@�{@Ͳ-@�V@�j@� �@�  @˾w@�t�@�S�@�
=@�{@�{@�/@���@ǥ�@�v�@���@�I�@��@���@�"�@�^5@��^@���@�b@�-@�v�@��
@��
@�p�@��-@�S�@�/@�X@�`B@��@�|�@��y@�"�@�\)@�;d@�^5@�V@��@�7L@�r�@��@�A�@���@��+@�x�@��@�t�@��;@��`@��`@���@���@��@��@��@�dZ@��@��/@��D@��D@��7@��@�5?@��h@��@�r�@�(�@��@���@�M�@��#@�p�@�j@�9X@��m@�t�@���@��\@�@���@���@�I�@���@�{@��^@�x�@�@�\)@���@��m@���@��
@��F@���@�  @���@�S�@��y@���@�
=@��@���@�=q@��h@�%@���@��@��/@��/@�%@��`@�j@�(�@�  @��
@��P@�\)@�33@�o@��@�ff@�-@��@��#@�hs@��`@��F@�\)@�;d@�;d@�33@�o@���@��\@�V@�{@���@���@���@��@�9X@���@�C�@�
=@��y@�~�@�E�@�{@��@���@��7@�j@�\)@�V@�=q@��@��@�b@�I�@�Q�@���@���@���@�1@�l�@�;d@��@���@�~�@�^5@���@��
@�Q�@��;@�ƨ@��@�;d@��@���@�ff@�J@���@��^@��7@�7L@�G�@���@�Z@�r�@�z�@�I�@��@��w@���@�33@�ȴ@�ff@�J@��-@�O�@�O�@��7@�X@��9@���@���@���@��@�7L@�X@�X@��@�z�@���@���@z-@s@i�@_�w@Y%@P�u@I�#@B�\@;��@6��@2��@-?}@'\)@!G�@O�@�@�F@�@�m@A�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A��A�bA�oA�VA�bA�JA�JA�
=A�VA�VA�
=A�
=A�
=A�
=A�
=A�JA�JA�JA�VA�VA�bA�VA�VA�bA�bA�bA�oA�oA�{A�{A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A���A��`A�VA�dZA�?}A�S�A�K�AœuA��mAA��A��\A���A���A�1A���A��wA��A�bNA�5?A��A��\A�ƨA�C�A���A�~�A�33A���A�ĜA��jA��^A�`BA��A�33A��A��A��`A��A��+A��A��A���A�XA��jA��+A��wA�oA���A�A�A��A��+A�-A�G�A�ĜA��A�A��#A�n�A��A��A���A��PA��A���A}S�A{"�Ax��Aw"�Au�PAq�
An5?Al1'Aj5?Ah�/AgXAa
=A]�AY��AVz�AR�AP(�AN^5AM&�ALn�AK�wAJ�HAH�yAG/AFA�AEt�AC�;A?\)A>A�A=��A;l�A:^5A:  A9p�A9�A8��A8(�A6E�A5C�A3"�A2I�A2bNA1��A0^5A/ƨA/&�A-+A+K�A)?}A(ZA(1'A'"�A&�A$��A#�A"�yA!��A!�A ��A �A v�AK�A��A�A/AoA�/AM�AoAO�A�uA��A��AƨA;dA  A(�Ap�AXA��A&�A
�yA
�AO�A�wA�A�hA�#AVAz�A��A�/A��AbNA�A��A�hA�Ap�A�`AQ�A��AK�A �+@��H@�;d@�@�ff@��@���@��@��9@�?}@��@���@�R@旍@���@���@�@�P@�M�@Ӆ@�ff@У�@�@��m@���@��`@�b@�1@ӍP@ҟ�@�x�@�j@�(�@�t�@�;d@�\)@�j@��y@�Q�@�hs@� �@���@؋D@ו�@�-@�`B@�Ĝ@ԣ�@�j@��m@�o@�V@�j@ύP@Χ�@Ώ\@Ώ\@�{@Ͳ-@�V@�j@� �@�  @˾w@�t�@�S�@�
=@�{@�{@�/@���@ǥ�@�v�@���@�I�@��@���@�"�@�^5@��^@���@�b@�-@�v�@��
@��
@�p�@��-@�S�@�/@�X@�`B@��@�|�@��y@�"�@�\)@�;d@�^5@�V@��@�7L@�r�@��@�A�@���@��+@�x�@��@�t�@��;@��`@��`@���@���@��@��@��@�dZ@��@��/@��D@��D@��7@��@�5?@��h@��@�r�@�(�@��@���@�M�@��#@�p�@�j@�9X@��m@�t�@���@��\@�@���@���@�I�@���@�{@��^@�x�@�@�\)@���@��m@���@��
@��F@���@�  @���@�S�@��y@���@�
=@��@���@�=q@��h@�%@���@��@��/@��/@�%@��`@�j@�(�@�  @��
@��P@�\)@�33@�o@��@�ff@�-@��@��#@�hs@��`@��F@�\)@�;d@�;d@�33@�o@���@��\@�V@�{@���@���@���@��@�9X@���@�C�@�
=@��y@�~�@�E�@�{@��@���@��7@�j@�\)@�V@�=q@��@��@�b@�I�@�Q�@���@���@���@�1@�l�@�;d@��@���@�~�@�^5@���@��
@�Q�@��;@�ƨ@��@�;d@��@���@�ff@�J@���@��^@��7@�7L@�G�@���@�Z@�r�@�z�@�I�@��@��w@���@�33@�ȴ@�ff@�J@��-@�O�@�O�@��7@�X@��9@���@���@���@��@�7L@�X@�X@��@�z�G�O�@���@z-@s@i�@_�w@Y%@P�u@I�#@B�\@;��@6��@2��@-?}@'\)@!G�@O�@�@�F@�@�m@A�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
�sB
�mB
�mB
�mB
�mB
�mB
�mB
�mB
�mB
�mB
�mB
�mB
�mB
�mB
�mB
�mB
�mB
�mB
�mB
�mB
�mB
�mB
�mB
�mB
�mB
�mB
�mB
�mB
�mB
�mB
�sB
�sB
�sB
�sB
�sB
�sB
�sB
�sB
�sB
�sB
�sB
�yB
�B
�B
�B1B��B�B+BF�BR�Bn�B�B� B��B�9B�}B��BŢB��B��B��B��B��B��B��B��B�B�B�TB�B�B��B�B�B�B�#BŢB�jB�9B�B��B��B��B��B�BjBXBJ�BB�B9XB&�B
=B�NB��B�9B�VBjB<jBPB
�B
B
��B
�\B
|�B
ffB
VB
;dB
$�B
+B	��B	�ZB	�B	ȴB	�!B	��B	�=B	|�B	p�B	aHB	;dB	#�B	PB��B�B�`B�BB�#B�B��B��BĜB�qB�RB�9B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B��BǮB��B��BɺB��B�XB�dBȴB��B�)B�sB�B�B�B�B�B�B�B�mB�B��B��B	  B	  B��B��B��B��B��B	B	hB	�B	bB	#�B	 �B	oB�mBǮB��B�B�fB�B	DB	+B	hB	oB	B�B��B��B��B	B	B	%B	�B	�B	�B	�B	{B	hB	uB	oB	�B	�B	�B	�B	�B	bB	  B�B��B�yB��B�LB��B�B��B��B�B��B�hB�\B��B��B��B��BȴBɺBɺB��B��B��B��B��B��B�#B�`B��B	B	\B	>wB	J�B	I�B	K�B	L�B	L�B	N�B	O�B	O�B	O�B	VB	Q�B	O�B	N�B	N�B	P�B	XB	YB	YB	[#B	]/B	]/B	_;B	bNB	cTB	e`B	ffB	ffB	hsB	e`B	gmB	hsB	o�B	m�B	iyB	bNB	]/B	ZB	YB	XB	W
B	[#B	XB	Q�B	\)B	_;B	[#B	aHB	m�B	{�B	|�B	|�B	}�B	}�B	~�B	� B	�B	�B	� B	}�B	�B	�B	�B	�7B	�7B	�7B	�+B	�B	�B	�B	�+B	�uB	�{B	�oB	�JB	�{B	��B	��B	�oB	�PB	�VB	�bB	�{B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�-B	�LB	�RB	�XB	�XB	�^B	�jB	ÖB	ŢB	ƨB	ǮB	ɺB	��B	��B	��B	��B	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�B	�)B	�)B	�/B	�5B	�;B	�;B	�;B	�;B	�HB	�TB	�TB	�`B	�fB	�fB	�fB	�TB	�;B	�)B	�5B	�5B	�#B	�B	�5B	�5B	�BB	�TB	�HB	�5B	�)B	�#B	�#B	�)B	�)B	�;B	�fB	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B	��B	��B
B
B
B
B
%B
+B
+B
B
B
B
�B
�B
�B
#�B
1'B
6FB
=qB
C�B
H�B
N�B
S�B
W
B
\)B
`BB
e`B
k�B
n�B
r�B
v�B
z�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
�kB
�dB
�fB
�fB
�hB
�hB
�gB
�gB
�hB
�gB
�fB
�fB
�gB
�hB
�hB
�fB
�hB
�gB
�fB
�gB
�gB
�gB
�fB
�fB
�gB
�gB
�fB
�fB
�gB
�gB
�hB
�hB
�kB
�jB
�kB
�hB
�jB
�jB
�jB
�hB
�jB
�rB
�yB
�yB
�wB)B��B��B BF�BR�Bn�B�B�B�|B�0B�rB�BŗBʷB��B��B��B��B��B��B��B��B�B�LB�rB�B��B�B�B�{B�BœB�_B�1B��B��B��B��B��B��BjuBXBJ�BB�B9LB&�B
.B�?B˻B�-B�IBjuB<`BBB
�B
B
��B
�TB
|�B
f_B
U�B
;]B
$�B
&B	��B	�VB	��B	ȯB	�B	��B	�;B	|�B	p�B	aHB	;bB	#�B	SB��B�B�eB�GB�)B�	B��B��BĢB�wB�XB�@B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B��BǯB��B��BɾB��B�]B�kBȹB��B�-B�vB�B�B�B�B�B�B�B�pB�B��B��B	 B��B��B��B��B��B��B	B	gB	�B	cB	#�B	 �B	lB�oBǰB��B�B�jB�B	CB	*B	hB	pB	B�B��B��B��B	B	B	$B	~B	�B	�B	�B	{B	hB	tB	lB	�B	�B	�B	�B	�B	cB��B�B��B�yB��B�NB��B�B��B��B�B��B�kB�aB��B��B��B��BȵBɽBɻB��B��B��B��B��B��B� B�aB��B	B	[B	>sB	J�B	I�B	K�B	L�B	L�B	N�B	O�B	O�B	O�B	U�B	Q�B	O�B	N�B	N�B	P�B	XB	YB	YB	[B	](B	]*B	_6B	bIB	cOB	e[B	f_B	f^B	hpB	eZB	ghB	hmB	o�B	m�B	isB	bIB	]*B	ZB	YB	X
B	WB	[B	XB	Q�B	\%B	_7B	[B	aBB	m�B	{�B	|�B	|�B	}�B	}�B	~�B	�B	�B	�B	�B	}�B	��B	�B	�B	�1B	�/B	�0B	�&B	�B	�B	�	B	�$B	�nB	�sB	�jB	�DB	�uB	��B	�xB	�iB	�KB	�OB	�\B	�uB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	� B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�%B	�BB	�IB	�MB	�OB	�UB	�dB	ÌB	ŜB	ƟB	ǧB	ɱB	˽B	��B	��B	ʻB	ȭB	ȫB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�%B	�+B	�0B	�1B	�2B	�0B	�<B	�KB	�KB	�UB	�\B	�]B	�[B	�LB	�0B	�B	�-B	�*B	�B	�B	�-B	�,B	�7B	�JB	�=B	�+B	�B	�B	�B	�B	�B	�1B	�\B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
B
B
B
B
"B
 B
G�O�B
B
�B
�B
�B
#�B
1B
6;B
=fB
C�B
H�B
N�B
S�B
V�B
\B
`5B
eSB
kwB
n�B
r�B
v�B
z�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.13 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436412016080714364120160807143641  AO  ARCAADJP                                                                    20151205031612    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20151205031612  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20151205031612  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143641  IP                  G�O�G�O�G�O�                