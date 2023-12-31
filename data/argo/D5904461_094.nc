CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-01-11T03:15:43Z AOML 3.0 creation; 2016-08-07T21:36:42Z UW 3.1 conversion     
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
resolution        :�o     �  U|   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _p   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gd   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  id   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qX   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yL   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {L   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �@   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �4   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �d   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �d   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �d   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �d   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20160111031543  20160807143642  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               ^A   AO  5286_8897_094                   2C  D   APEX                            6531                            072314                          846 @׌�A�I`1   @׌��b�"@2���E��cCC��%1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    ^A   B   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B ffB(ffB0  B8  B@  BH  BP  BW��B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DL��DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dy�3D���D�\�D�� D�� D�fD�9�D�s3D��3D�  D�0 D�� D�� D��3D�FfDڌ�D��3D�3D�6fD� D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�\)@�(�A{A"{AB{Ab{A�
=A�
=A�
=A�
=A�
=A�
=A�
=A�
=B �B�B�B�B �B(�B0�B8�B@�BH�BP�BX�B`�Bh�Bp�Bx�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�u�B�B�B�B�B�B�B�\B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C !HC!HC!HC!HC!HC
!HC!HC!HC!HC!HC!HC!HC!HC!HC!HC!HC !HC"!HC$!HC&!HC(!HC*!HC,!HC.!HC0!HC2!HC4!HC6!HC8!HC:!HC<!HC>!HC@!HCB!HCD!HCF!HCH!HCJ!HCL!HCN!HCP!HCR!HCT!HCV!HCX!HCZ!HC\!HC^!HC`!HCb!HCd!HCf!HCh!HCj!HCl!HCn!HCp!HCr!HCt!HCv!HCx!HCz!HC|!HC~!HC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D RD �RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RD	RD	�RD
RD
�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RD RD �RD!RD!�RD"RD"�RD#RD#�RD$RD$�RD%RD%�RD&RD&�RD'RD'�RD(RD(�RD)RD)�RD*RD*�RD+RD+�RD,RD,�RD-RD-�RD.RD.�RD/RD/�RD0RD0�RD1RD1�RD2RD2�RD3RD3�RD4RD4�RD5RD5�RD6RD6�RD7RD7�RD8RD8�RD9RD9�RD:RD:�RD;RD;�RD<RD<�RD=RD=�RD>RD>�RD?RD?�RD@RD@�RDARDA�RDBRDB�RDCRDC�RDDRDD�RDERDE�RDFRDF�RDGRDG�RDHRDH�RDIRDI�RDJRDJ�RDKRDK�RDLRDL�RDM�DM�RDNRDN�RDORDO�RDPRDP�RDQRDQ�RDRRDR�RDSRDS�RDTRDT�RDURDU�RDVRDV�RDWRDW�RDXRDX�RDYRDY�RDZRDZ�RD[RD[�RD\RD\�RD]RD]�RD^RD^�RD_RD_�RD`RD`�RDaRDa�RDbRDb�RDcRDc�RDdRDd�RDeRDe�RDfRDf�RDgRDg�RDhRDh�RDiRDi�RDjRDj�RDkRDk�RDlRDl�RDmRDm�RDnRDn�RDoRDo�RDpRDp�RDqRDq�RDrRDr�RDsRDs�RDtRDt��Dy��D� �D�`�D��)D��)D�
�D�=�D�w\D��\D�)D�4)D��)D��)D��\D�J�Dڐ�D��\D�\D�:�D�)D��)11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��yA��#A���A�ĜA���AʾwAʾwAʾwAʾwAʾwAʾwAʾwAʼjAʺ^Aʴ9AʬAʧ�Aʣ�AʓuAʅA�~�A�5?A���A�p�A���A�bA�=qA�+A�/A���AƼjAơ�AƅA�|�AƅA�r�A�VA�K�A�A�A�1'A�Q�A�ffA�5?A�33A�;dA�ffA�ZAƏ\A�ZAå�A�-A��A��#A�bNA�S�A���A�/A�?}A��!A��A�dZA��RA���A�z�A��A�XA���A���A�33A�ƨA�E�A�  A�l�A�
=A��A� �A�A�^5A��+A�$�A��A���A��wA�hsA��`A���A�r�A��A�K�A�M�A���A��A�&�A���A�A���A�1'A�Q�A��PA��+A��A�$�A;dA|^5AzQ�Ay�^Ax�yAt��Aop�Al�uAkO�AjȴAjJAh��Af^5A`�HA[�
AWVAT�AR��AO?}AL��AJbNAIXAH  AG"�AFr�AD9XAA�;A?��A=��A<A;�PA;dZA:�A9O�A6(�A4I�A3��A2^5A133A0�9A/ƨA.��A,��A+�^A++A*M�A)7LA(�yA(��A(n�A({A'33A&bNA%�hA"�jA!+A ��A-AZA�A�RAXA{A"�AjA�^A�`A�
AE�A�AQ�AAp�A��A��Ap�A
�/A
�uA	��A9XA{A�A��A��A$�A�wA;dA (�@���@���@��T@�$�@��/@��@��@�$�@�{@��^@��@�^5@���@��+@�@���@�(�@��@�@�M�@��
@�w@�F@�x�@�P@�|�@�5?@��D@��@߅@�;d@�t�@���@���@�@�v�@�E�@��@Ցh@�Z@��@��m@�1@�(�@�  @�o@��@ӥ�@ӥ�@љ�@��y@͉7@���@��@�v�@ɡ�@ə�@ɲ-@�@ə�@�  @�b@��@ƸR@��@�  @�33@��R@�E�@�&�@�1@��P@�+@�ȴ@�v�@��@�O�@��@���@�n�@��@���@���@��h@��u@�bN@��;@�"�@��R@��@���@�%@��@��\@�^5@�^5@��@�%@��`@���@��@�ƨ@�C�@��@���@���@�n�@�-@��@��#@��^@�X@�/@�?}@�&�@���@�Q�@��P@��@���@�-@���@�G�@���@�Z@�9X@�j@��
@���@��\@�V@�{@��7@�@�=q@��h@��^@���@��`@���@�I�@��@��@��P@��@��w@���@�C�@��+@�{@��T@���@���@�Q�@��@�  @�ƨ@�|�@�+@��!@�n�@��@�@��@�O�@���@�z�@��w@�t�@�dZ@�S�@�+@���@�5?@��h@�X@�&�@�Q�@��;@��w@���@�l�@�dZ@�\)@��@��@�ȴ@�v�@�M�@��@�{@��@�$�@�@�p�@�hs@�V@�Ĝ@�G�@���@���@��7@���@��@�V@�=q@�{@���@���@�G�@��@��`@��j@��j@��D@���@�|�@��@���@���@���@��+@�M�@�=q@��T@���@��7@�G�@�/@�%@��`@�bN@�(�@�1@���@��@�S�@��y@���@�=q@��-@��@�p�@�/@���@���@�9X@���@���@�K�@�
=@�$�@��T@�hs@���@��9@��@��9@��j@���@���@��`@��/@��`@��`@��@��@��/@�Ĝ@��D@�A�@�1@��w@�dZ@�@��H@��@�ȴ@��!@���@�v�@�ff@�5?@�{@���@�?}@��@���@��@�z�@�(�@l�@~E�@}��@}p�@}�@|�j@|��@y7L@m�T@b��@W��@Q�7@KC�@D�@>�y@8b@2�\@-`B@( �@#C�@�T@bN@��@�\@  @��@\)@S�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A��yA��#A���A�ĜA���AʾwAʾwAʾwAʾwAʾwAʾwAʾwAʼjAʺ^Aʴ9AʬAʧ�Aʣ�AʓuAʅA�~�A�5?A���A�p�A���A�bA�=qA�+A�/A���AƼjAơ�AƅA�|�AƅA�r�A�VA�K�A�A�A�1'A�Q�A�ffA�5?A�33A�;dA�ffA�ZAƏ\A�ZAå�A�-A��A��#A�bNA�S�A���A�/A�?}A��!A��A�dZA��RA���A�z�A��A�XA���A���A�33A�ƨA�E�A�  A�l�A�
=A��A� �A�A�^5A��+A�$�A��A���A��wA�hsA��`A���A�r�A��A�K�A�M�A���A��A�&�A���A�A���A�1'A�Q�A��PA��+A��A�$�A;dA|^5AzQ�Ay�^Ax�yAt��Aop�Al�uAkO�AjȴAjJAh��Af^5A`�HA[�
AWVAT�AR��AO?}AL��AJbNAIXAH  AG"�AFr�AD9XAA�;A?��A=��A<A;�PA;dZA:�A9O�A6(�A4I�A3��A2^5A133A0�9A/ƨA.��A,��A+�^A++A*M�A)7LA(�yA(��A(n�A({A'33A&bNA%�hA"�jA!+A ��A-AZA�A�RAXA{A"�AjA�^A�`A�
AE�A�AQ�AAp�A��A��Ap�A
�/A
�uA	��A9XA{A�A��A��A$�A�wA;dA (�@���@���@��T@�$�@��/@��@��@�$�@�{@��^@��@�^5@���@��+@�@���@�(�@��@�@�M�@��
@�w@�F@�x�@�P@�|�@�5?@��D@��@߅@�;d@�t�@���@���@�@�v�@�E�@��@Ցh@�Z@��@��m@�1@�(�@�  @�o@��@ӥ�@ӥ�@љ�@��y@͉7@���@��@�v�@ɡ�@ə�@ɲ-@�@ə�@�  @�b@��@ƸR@��@�  @�33@��R@�E�@�&�@�1@��P@�+@�ȴ@�v�@��@�O�@��@���@�n�@��@���@���@��h@��u@�bN@��;@�"�@��R@��@���@�%@��@��\@�^5@�^5@��@�%@��`@���@��@�ƨ@�C�@��@���@���@�n�@�-@��@��#@��^@�X@�/@�?}@�&�@���@�Q�@��P@��@���@�-@���@�G�@���@�Z@�9X@�j@��
@���@��\@�V@�{@��7@�@�=q@��h@��^@���@��`@���@�I�@��@��@��P@��@��w@���@�C�@��+@�{@��T@���@���@�Q�@��@�  @�ƨ@�|�@�+@��!@�n�@��@�@��@�O�@���@�z�@��w@�t�@�dZ@�S�@�+@���@�5?@��h@�X@�&�@�Q�@��;@��w@���@�l�@�dZ@�\)@��@��@�ȴ@�v�@�M�@��@�{@��@�$�@�@�p�@�hs@�V@�Ĝ@�G�@���@���@��7@���@��@�V@�=q@�{@���@���@�G�@��@��`@��j@��j@��D@���@�|�@��@���@���@���@��+@�M�@�=q@��T@���@��7@�G�@�/@�%@��`@�bN@�(�@�1@���@��@�S�@��y@���@�=q@��-@��@�p�@�/@���@���@�9X@���@���@�K�@�
=@�$�@��T@�hs@���@��9@��@��9@��j@���@���@��`@��/@��`@��`@��@��@��/@�Ĝ@��D@�A�@�1@��w@�dZ@�@��H@��@�ȴ@��!@���@�v�@�ff@�5?@�{@���@�?}@��@���@��@�z�@�(�@l�@~E�@}��@}p�@}�@|�jG�O�@y7L@m�T@b��@W��@Q�7@KC�@D�@>�y@8b@2�\@-`B@( �@#C�@�T@bN@��@�\@  @��@\)@S�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
"�B
%�B
/B
E�B
jB
��B
�TB{B$�B_;Bz�B�1B� B�B�7B�uB��B��B��B��B��B��B��B�'B�wB�dB��B��B�B�B��BB��BVB  B��B��BJBhB�B)�B?}B49B=qB%B�B�B�B�B�B��B��B��B  BB��B�yB�B��B�wB�B��B��B�{B�PB|�BZB@�B2-B5?B>wB33B�BB
�`B
�jB
�hB
t�B
[#B
2-B
\B
B	�B	�TB	��B	�RB	��B	�uB	�DB	� B	ffB	@�B	'�B	�B	�B	{B	
=B��B�/B��B��B��B�{B�+B}�Bv�Bt�Br�Bo�Bk�BdZB\)BS�BO�BP�BO�BN�BN�BN�BQ�BXBXB]/B_;B_;B_;B`BBdZBffBgmBjBn�B�B�\B�hB�VB�uB��B�{B�uB�{B�uB�PB��B��B��B��B�hBk�BYBVBW
BXBVBT�BVBVBYB[#Be`BffBgmBjBk�BdZBgmBhsBiyBjBk�Bl�Bk�Bn�Br�Bw�Bx�Bv�Bs�Br�Bw�B}�B�B�B�B�DB��B��B��B��B��B��B��B��B�bB�Br�Bq�Bo�Bs�Bt�Bz�B}�B~�B�B�1B�oB��B�B��B��B�\B�%B�B�B�B�B�+B�1B�+B�=B�hB�{B�bB�PB�\B�bB�\B�oB��B��B��B�'B�FB�9B�?B�?B�-B�B��B��B�B�B�3B�LB�RB�RB�^B�dB�qB�}BĜBȴB��B��B��B��B��B�
B�HB�fB�B�B�B��B��B	B	%B	
=B	
=B	DB	\B	bB	oB	{B	�B	�B	�B	�B	 �B	!�B	$�B	&�B	'�B	(�B	-B	1'B	2-B	49B	5?B	9XB	;dB	?}B	C�B	J�B	M�B	O�B	P�B	R�B	T�B	VB	[#B	`BB	aHB	dZB	gmB	hsB	l�B	w�B	x�B	z�B	y�B	w�B	x�B	{�B	}�B	�B	�B	�B	�%B	�+B	�1B	�7B	�DB	�DB	�JB	�\B	�oB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�!B	�-B	�9B	�FB	�FB	�FB	�XB	�dB	�qB	�}B	��B	��B	��B	��B	ÖB	ÖB	ŢB	ǮB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�5B	�HB	�TB	�TB	�`B	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
  B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
1B
1B
	7B
	7B
	7B
	7B
	7B
	7B
	7B
	7B

=B
DB
JB
JB
JB
PB
PB
VB
\B
bB
bB
bB
hB
hB
hB
hB
�B
#�B
.B
33B
9XB
?}B
C�B
I�B
N�B
VB
ZB
^5B
cTB
hsB
l�B
n�B
o�B
u�B
z�B
~�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
"�B
%�B
/B
E�B
jzB
��B
�LBsB$�B_1Bz�B�)B�B��B�-B�kB��B��B��B��B��B��B��B�B�jB�ZB�~B˽B�B�B��B	B��BJB��B��BʵB<BZB�B)�B?sB41B=gBB�B�yB�yB�B�B��B��B��B��BB��B�mB�B��B�kB��B��B��B�qB�CB|�BZB@zB2#B52B>mB3(B�BB
�VB
�`B
�cB
t�B
[B
2(B
TB
B	�B	�MB	��B	�NB	��B	�rB	�BB	�B	feB	@�B	'�B	�B	�B	�B	
@B��B�6B��B��B��B��B�6B}�Bv�Bt�Br�Bo�Bk�BdbB\4BTBO�BP�BO�BN�BN�BN�BQ�BXBXB]7B_EB_DB_EB`LBddBfpBguBj�Bn�B�$B�dB�oB�]B�~B��B��B�{B��B�zB�UB��B��B��B��B�mBk�BY BVBWBXBVBUBVBVBYB[+BejBfoBgtBj�Bk�BdbBgvBh|Bi�Bj�Bk�Bl�Bk�Bn�Br�Bw�Bx�Bv�Bs�Br�Bw�B}�B�B�B�"B�HB��B��B��B��B��B��B��B��B�gB�Br�Bq�Bo�Bs�Bt�Bz�B}�B B�B�6B�tB��B�B��B��B�aB�(B�B�B�B�"B�/B�6B�0B�BB�kB��B�fB�UB�^B�fB�aB�rB��B��B� B�*B�FB�<B�AB�AB�.B�B��B��B�B�B�3B�NB�SB�QB�^B�eB�oB�~BĜBȳB��B��B��B��B��B�
B�HB�dB�B�B�B��B��B	B	"B	
;B	
:B	AB	ZB	bB	kB	yB	�B	�B	�B	�B	 �B	!�B	$�B	&�B	'�B	(�B	-	B	1#B	2*B	47B	5;B	9SB	;`B	?yB	C�B	J�B	M�B	O�B	P�B	R�B	T�B	U�B	[B	`=B	aBB	dUB	ggB	hnB	l�B	w�B	x�B	z�B	y�B	w�B	x�B	{�B	}�B	�B	�
B	�B	�B	�$B	�*B	�0B	�=B	�>B	�CB	�TB	�fB	�sB	�wB	�xB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�$B	�1B	�=B	�>B	�;B	�PB	�\B	�iB	�uB	�|B	�{B	�{B	��B	ÍB	ÌB	řB	ǨB	ʹB	˼B	��B	��B	��B	��B	��B	��B	�B	�+B	�@B	�GB	�LB	�XB	�gB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B	��B	��B	��B	��B	��B	��B
 �B
 B
B
B
B
B
B
B
B
B
B
B
B
B
B
 B
#B
$B
%B
&B
	,B
	+B
	,B
	-B
	-B
	-B
	-B
	+B

4B
:B
=B
@B
=B
GB
DB
KB
QB
WB
XB
WB
]B
]G�O�B
]B
�B
#�B
.	B
3'B
9MB
?qB
C�B
I�B
N�B
U�B
ZB
^'B
cIB
heB
l~B
n�B
o�B
u�B
z�B
~�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.13 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436422016080714364220160807143642  AO  ARCAADJP                                                                    20160111031543    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160111031543  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160111031543  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143642  IP                  G�O�G�O�G�O�                