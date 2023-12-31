CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-04-30T19:17:08Z AOML 3.0 creation; 2016-08-07T21:36:46Z UW 3.1 conversion     
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
_FillValue                 �  A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  K|   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  Mx   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  Uh   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]X   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _T   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  gD   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  y    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �(   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �(   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �(   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �(   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �T   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �X   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �\   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �`   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �d   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20160430191708  20160807143646  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               sA   AO  5286_8897_115                   2C  D   APEX                            6531                            072314                          846 @ר�N��a1   @ר��s�J@47�O�;d�cS�z�H1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    sA   B   B   @�33@�  @���AffA@  A`  A���A���A�  A�  A���A�33A�  A�  A�33B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!fD!�fD"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DNfDN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtY�Dy��D��D�FfD�y�D�ٚD� D�FfD���D�� D�	�D�9�D�,�D��fD� D�FfD�|�D๚D�3D�L�D�y�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�\)@�(�A z�A z�AB{Ab{A��
A��
A�
=A�
=A��
A�=pA�
=A�
=B �B�B�B�B �B(�B0�B8�B@�BH�BP�BX�B`�Bh�Bp�Bx�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C !HC!HC!HC!HC!HC
!HC!HC!HC!HC!HC!HC!HC!HC!HC!HC!HC !HC"!HC$!HC&!HC(!HC*!HC,!HC.!HC0!HC2!HC4!HC6!HC8!HC:!HC<!HC>!HC@!HCB!HCD!HCF!HCH!HCJ!HCL!HCN!HCP!HCR!HCT!HCV!HCX!HCZ!HC\!HC^!HC`!HCb!HCd!HCf!HCh!HCj!HCl!HCn!HCp!HCr!HCt!HCv!HCx!HCz!HC|!HC~!HC��C��C��C��C��C��C��C��C��C��C��C��C��C�qC�qC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D RD �RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RD	RD	�RD
RD
�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RD RD �RD!�D!��D"RD"�RD#RD#�RD$RD$�RD%RD%�RD&RD&�RD'RD'�RD(RD(�RD)RD)�RD*RD*�RD+RD+�RD,RD,�RD-RD-�RD.RD.�RD/RD/�RD0RD0�RD1RD1�RD2RD2�RD3RD3�RD4RD4�RD5RD5�RD6RD6�RD7RD7�RD8RD8�RD9RD9�RD:RD:�RD;RD;�RD<RD<�RD=RD=�RD>RD>�RD?RD?�RD@RD@�RDARDA�RDBRDB�RDCRDC�RDDRDD�RDERDE�RDFRDF�RDGRDG�RDHRDH�RDIRDI�RDJRDJ�RDKRDK�RDLRDL�RDMRDM�RDN�DN�RDORDO�RDPRDP�RDQRDQ�RDRRDR�RDSRDS�RDTRDT�RDURDU�RDVRDV�RDWRDW�RDXRDX�RDYRDY�RDZRDZ�RD[RD[�RD\RD\�RD]RD]�RD^RD^�RD_RD_�RD`RD`�RDaRDa�RDbRDb�RDcRDc�RDdRDd�RDeRDe�RDfRDf�RDgRDg�RDhRDh�RDiRDi�RDjRDj�RDkRDk�RDlRDl�RDmRDm�RDnRDn�RDoRDo�RDpRDp�RDqRDq�RDrRDr�RDsRDs�RDtRDta�Dy�D��D�J�D�}�D���D�)D�J�D���D��)D��D�=�D�0�D�ڏD�)D�J�Dڀ�D��D�\D�P�D�}�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�`BA�`BA�S�A�\)A�^5A�\)A�bNA�\)A�XA�A�A��A�
=A���Aɛ�A�$�A�1'Aǧ�AǓuAǍPA�5?A�$�AŅA��mAę�AąA�p�A�ffA�Q�A��A�ȴA�x�A���A�E�A�9XA��A�bNA��A��mA��
A���A�ĜA�n�A�+A� �A��A�JA��
A�x�A�bNA�O�A��A��PA�(�A�A���A��A�|�A��A���A��jA�VA��^A��A��/A�C�A��`A���A�(�A��+A��jA���A���A��A�A�K�A�(�A�ȴA�K�A��7A��DA�$�A���A���A�?}A���A��DA�|�A��A�~�A��`A�oA�VA�^5A���A��A�?}A��-A�n�A���A�O�A�\)A�p�A�G�A�S�A��`A�G�A�;dA� �A��A�;dA��7A�/A��A��!A�A�A~��A}"�Ay�mAy+Aw��As�hAq%AoVAi��AdQ�A_x�A\�\AY��ARz�AO�AL��AJ~�AH�AEoAB�A?��A?�-A?��A>  A<M�A;�
A;x�A;�A8�uA5hsA4A2�A133A/�FA.�A-�A,��A+��A+A*�`A*��A)`BA)"�A)��A(Q�A&��A$1'A#�A"�+A!�#A!/A ��A�PAC�AoA�A�TA+A��A��A5?AO�A��A{A�;A�AA;dA��A�7AdZA�A��AffA�Ax�A  A?}A&�A	p�An�A��AƨA��A�A��A�A��A%A ĜA j@��@�ff@���@�?}@��/@��/@�7L@�x�@���@���@�M�@�"�@�7L@��y@�r�@��
@��@��H@�R@�E�@�=q@��@��@��@��#@�ff@��@�1'@㝲@��@�R@�@��T@ݙ�@݉7@ݑh@�/@�Z@ڸR@�E�@�V@�Ĝ@�j@�ƨ@֧�@ՙ�@�j@��;@�l�@�t�@�|�@�l�@Ұ!@ѡ�@�7L@�%@�bN@�C�@�@��@̼j@�1@�dZ@�5?@�hs@ȴ9@�l�@�dZ@���@�/@ļj@�Z@��m@�@�p�@��
@�"�@���@�$�@�M�@�M�@�M�@��T@�p�@�O�@���@�V@���@��w@�+@��!@�v�@�^5@�n�@���@�J@���@��@�+@�1'@�S�@�@�ff@�@�z�@�I�@���@�ƨ@��@�"�@���@��;@��H@�@���@�Ĝ@��9@��@�A�@�t�@�
=@�ff@�E�@��h@��u@�ƨ@��P@�S�@�33@��y@��!@���@��y@��H@��@��y@��H@�ȴ@�~�@�=q@�5?@�J@�M�@�~�@�=q@�`B@�`B@�G�@�7L@���@��@��`@��/@���@�&�@�7L@�`B@��T@���@���@�M�@��T@���@�p�@�?}@��@�+@��@�Ĝ@��m@�t�@�t�@�dZ@�;d@�K�@�C�@��@��!@�~�@�@��T@��^@���@�O�@��@��u@�I�@�1'@��@�  @���@�t�@�
=@��@��@��R@��!@��!@���@��+@�V@�V@�5?@���@�`B@���@���@�I�@���@��@�A�@�S�@�=q@�X@��@��\@��\@��R@�E�@��^@�?}@��j@���@��;@���@�|�@��\@�E�@��@��-@���@��@�X@�G�@�/@��9@�9X@���@��w@���@�t�@�dZ@�;d@��@�
=@��@��!@��!@���@�E�@�-@�$�@��@���@���@��7@�X@�/@���@��@��/@��j@��u@�Z@�9X@� �@�1@��@��@�33@�o@���@��H@���@��!@�n�@�^5@�M�@�$�@�{@���@�p�@�G�@�I�@x�@o�@f{@^V@T��@L�@E`B@>@8  @3�F@,�/@(�9@$�D@!��@t�@�@��@?}1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111A�`BA�`BA�S�A�\)A�^5A�\)A�bNA�\)A�XA�A�A��A�
=A���Aɛ�A�$�A�1'Aǧ�AǓuAǍPA�5?A�$�AŅA��mAę�AąA�p�A�ffA�Q�A��A�ȴA�x�A���A�E�A�9XA��A�bNA��A��mA��
A���A�ĜA�n�A�+A� �A��A�JA��
A�x�A�bNA�O�A��A��PA�(�A�A���A��A�|�A��A���A��jA�VA��^A��A��/A�C�A��`A���A�(�A��+A��jA���A���A��A�A�K�A�(�A�ȴA�K�A��7A��DA�$�A���A���A�?}A���A��DA�|�A��A�~�A��`A�oA�VA�^5A���A��A�?}A��-A�n�A���A�O�A�\)A�p�A�G�A�S�A��`A�G�A�;dA� �A��A�;dA��7A�/A��A��!A�A�A~��A}"�Ay�mAy+Aw��As�hAq%AoVAi��AdQ�A_x�A\�\AY��ARz�AO�AL��AJ~�AH�AEoAB�A?��A?�-A?��A>  A<M�A;�
A;x�A;�A8�uA5hsA4A2�A133A/�FA.�A-�A,��A+��A+A*�`A*��A)`BA)"�A)��A(Q�A&��A$1'A#�A"�+A!�#A!/A ��A�PAC�AoA�A�TA+A��A��A5?AO�A��A{A�;A�AA;dA��A�7AdZA�A��AffA�Ax�A  A?}A&�A	p�An�A��AƨA��A�A��A�A��A%A ĜA j@��@�ff@���@�?}@��/@��/@�7L@�x�@���@���@�M�@�"�@�7L@��y@�r�@��
@��@��H@�R@�E�@�=q@��@��@��@��#@�ff@��@�1'@㝲@��@�R@�@��T@ݙ�@݉7@ݑh@�/@�Z@ڸR@�E�@�V@�Ĝ@�j@�ƨ@֧�@ՙ�@�j@��;@�l�@�t�@�|�@�l�@Ұ!@ѡ�@�7L@�%@�bN@�C�@�@��@̼j@�1@�dZ@�5?@�hs@ȴ9@�l�@�dZ@���@�/@ļj@�Z@��m@�@�p�@��
@�"�@���@�$�@�M�@�M�@�M�@��T@�p�@�O�@���@�V@���@��w@�+@��!@�v�@�^5@�n�@���@�J@���@��@�+@�1'@�S�@�@�ff@�@�z�@�I�@���@�ƨ@��@�"�@���@��;@��H@�@���@�Ĝ@��9@��@�A�@�t�@�
=@�ff@�E�@��h@��u@�ƨ@��P@�S�@�33@��y@��!@���@��y@��H@��@��y@��H@�ȴ@�~�@�=q@�5?@�J@�M�@�~�@�=q@�`B@�`B@�G�@�7L@���@��@��`@��/@���@�&�@�7L@�`B@��T@���@���@�M�@��T@���@�p�@�?}@��@�+@��@�Ĝ@��m@�t�@�t�@�dZ@�;d@�K�@�C�@��@��!@�~�@�@��T@��^@���@�O�@��@��u@�I�@�1'@��@�  @���@�t�@�
=@��@��@��R@��!@��!@���@��+@�V@�V@�5?@���@�`B@���@���@�I�@���@��@�A�@�S�@�=q@�X@��@��\@��\@��R@�E�@��^@�?}@��j@���@��;@���@�|�@��\@�E�@��@��-@���@��@�X@�G�@�/@��9@�9X@���@��w@���@�t�@�dZ@�;d@��@�
=@��@��!@��!@���@�E�@�-@�$�@��@���@���@��7@�X@�/@���@��@��/@��j@��u@�Z@�9X@� �@�1@��@��@�33@�o@���@��H@���@��!@�n�@�^5@�M�@�$�@�{@���G�O�@�G�@�I�@x�@o�@f{@^V@T��@L�@E`B@>@8  @3�F@,�/@(�9@$�D@!��@t�@�@��@?}1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	�
B	�B	��B	�B	�
B	�/B	�5B	�B	�mB	��B
�B
,B
P�B
ffB
�DB
��B  B�B#�B-B?}BT�BgmBq�Bv�B{�B�B�+B�hB��B�B�#B�B�B�B��B��BBBBBBB+B1BJB�B1'B49B7LBO�Bn�B�DB�BǮB�BB�B�B��B��B��B��B��B��B��B�B�B�ZB�
B��B��B��B�XB�B��B��B��B��B�hB{�Bm�BgmB\)BG�B-B�B�BbB��B��BĜB�3B��B~�Bo�BP�BF�B@�B5?B�BDB
�B
ǮB
�^B
�-B
��B
�1B
u�B
l�B
dZB
YB
D�B
49B
,B
#�B
{B
B	�B	�mB	�;B	B	�B	�{B	ffB	9XB	�B	B�B�BǮB�dB�!B��B��B�\B�DB�DB�DB�7B�7B�=B�1B�B{�Bx�B|�B�+B�7B�bB�\B�PB�DB�7B�7B�hB��B�oB��B�XB�LB�-B�B��B��B��B��B��B��B��B��B��B��B��B��B�'B�RB�RB�^B�XB�RB�dB�}BBȴB��BɺB�}B�'B�B�B��B��B��B��B��B��B��B��B��B��B��B��B�hB�oB�oB�bB�oB�{B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�!B�!B�3B��B�HB�HB�BB��B�qB�LB�FB�?B�9B�LB�qB�qB�qB�qB�qB�qB�wB�wB��B��BBÖBƨBǮB��B��B��B��B��B��B�
B�B�5B�BB�BB�BB�NB�ZB�fB�mB�yB�B�B�B��B	B	B	B	B	B	B	B	B	B	B	B	%B	JB	\B	bB	bB	bB	bB	bB	oB	{B	�B	�B	�B	$�B	)�B	,B	1'B	49B	33B	33B	33B	/B	.B	.B	-B	-B	-B	-B	1'B	7LB	;dB	D�B	I�B	M�B	P�B	P�B	W
B	\)B	^5B	^5B	^5B	bNB	aHB	aHB	cTB	dZB	e`B	hsB	jB	k�B	k�B	m�B	r�B	t�B	v�B	w�B	x�B	y�B	z�B	z�B	z�B	{�B	{�B	|�B	�B	�+B	�1B	�7B	�7B	�=B	�JB	�PB	�PB	�PB	�PB	�VB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�9B	�?B	�FB	�LB	�LB	�RB	�^B	�jB	�wB	B	ƨB	ɺB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�#B	�`B	�`B	�sB	�yB	�sB	�sB	�sB	�sB	�mB	�mB	�mB	�ZB	�TB	�TB	�TB	�TB	�TB	�TB	�TB	�ZB	�mB	�sB	�sB	�yB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
1B
bB
�B
"�B
+B
7LB
=qB
@�B
F�B
K�B
O�B
W
B
[#B
_;B
bNB
gmB
m�B
q�B
t�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111B	�	B	�B	��B	�B	�	B	�/B	�4B	�B	�lB	��B
�B
,B
P�B
f]B
�>B
��B
��B�B#�B-B?tBT�BgcBq�Bv�B{�B��B� B�^B��B�B�B�uB�~B�B��B��BB	BB B BB B"B@B�B1B4.B7@BO�Bn�B�7B�BǢB�6B�B�B��B��B��B��B��B��B��B�B�B�SB� B��BʷB�}B�MB�B��B��B��B��B�]B{�Bm�BgbB\BG�B-B�B�BTB��B��BēB�'B��B~�Bo�BP�BF�B@yB55B�B8B
�vB
ǣB
�UB
�&B
��B
�(B
u�B
l�B
dQB
YB
D�B
42B
,B
#�B
tB
B	�B	�gB	�7B	B	�B	�{B	feB	9YB	�B	B�B�BǲB�lB�)B��B��B�eB�LB�MB�NB�@B�?B�CB�6B�B{�Bx�B|�B�5B�AB�jB�dB�WB�MB�=B�>B�pB��B�tB��B�\B�RB�3B�B��B��B��B��B��B��B��B��B��B��B��B��B�+B�WB�TB�aB�]B�UB�iB��BBȷB��BɽB��B�,B�B�B��B��B��B��B��B��B��B��B��B��B��B��B�oB�vB�sB�gB�tB��B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�#B�#B�6B��B�HB�KB�CB��B�tB�NB�EB�BB�9B�LB�rB�tB�tB�rB�rB�sB�xB�wB��B��BB×BƩBǮB��B��B��B��B��B��B�
B�B�5B�BB�AB�BB�NB�YB�eB�lB�xB�B�B�B��B	B	B	B	B	B	B	B	B	B	B	B	"B	IB	ZB	aB	`B	`B	aB	_B	lB	zB	�B	�B	�B	$�B	)�B	,B	1$B	45B	30B	32B	30B	/B	.B	.B	-B	-B	-	B	-B	1#B	7FB	;bB	D�B	I�B	M�B	P�B	P�B	WB	\$B	^-B	^/B	^1B	bHB	aBB	aEB	cMB	dVB	eZB	hnB	jxB	kB	k~B	m�B	r�B	t�B	v�B	w�B	x�B	y�B	z�B	z�B	z�B	{�B	{�B	|�B	�B	�#B	�+B	�0B	�/B	�7B	�BB	�HB	�IB	�HB	�IB	�NB	�bB	�xB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�/B	�4B	�=B	�AB	�AB	�JB	�WB	�bB	�pB	B	ƠB	ɱB	˽B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�UB	�UB	�hB	�pB	�jB	�lB	�iB	�kB	�aB	�dB	�bB	�QB	�LB	�JB	�IB	�KB	�LB	�HB	�IB	�QB	�bB	�iB	�hB	�oB	�qB	�mB	�qB	�qB	�uB	�wB	�tB	�|B	�{B	�|B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��G�O�B	��B
'B
WB
�B
"�B
*�B
7@B
=fB
@uB
F�B
K�B
O�B
V�B
[B
_,B
bBB
gaB
m�B
q�B
t�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.13 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436462016080714364620160807143646  AO  ARCAADJP                                                                    20160430191708    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160430191708  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160430191708  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143646  IP                  G�O�G�O�G�O�                