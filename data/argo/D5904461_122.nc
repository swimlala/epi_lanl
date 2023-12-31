CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-06-06T19:15:52Z AOML 3.0 creation; 2016-08-07T21:36:47Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20160606191552  20160825183352  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               zA   AO  5286_8897_122                   2C  D   APEX                            6531                            072314                          846 @ױ�z�W@1   @ױ�`�@4�9XbN�cA�"��`1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    zA   B   B   @���@���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  BxffB�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd�Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy��D�3D�L�D���D�� D�  D�FfD�s3D�� D�fD�FfD���D�� D���D�@ D�vfD��fD��fD�,�D�|�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�@���A{A"{AB{Ab{A�
=A�
=A�
=A�
=A�
=A�
=A�
=A�
=B �B�B�B�B �B(�B0�B8�B@�BH�BP�BX�B`�Bh�Bp�Bx�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C !HC!HC!HC!HC!HC
!HC!HC!HC!HC!HC!HC!HC!HC!HC!HC!HC !HC"!HC$!HC&!HC(!HC*!HC,!HC.!HC0!HC2!HC4!HC6!HC8!HC:!HC<!HC>!HC@!HCB!HCD!HCF!HCH!HCJ!HCL!HCN!HCP!HCR!HCT!HCV!HCX!HCZ!HC\!HC^!HC`!HCb!HCd:�Cf!HCh!HCj!HCl!HCn!HCp!HCr!HCt!HCv!HCx!HCz!HC|!HC~!HC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�qC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D RD �RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RD	RD	�RD
RD
�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RD RD �RD!RD!�RD"RD"�RD#RD#�RD$RD$�RD%RD%�RD&RD&�RD'RD'�RD(RD(�RD)RD)�RD*RD*�RD+RD+�RD,RD,�RD-RD-�RD.RD.�RD/RD/�RD0RD0�RD1RD1�RD2RD2�RD3RD3�RD4RD4�RD5RD5�RD6RD6�RD7RD7�RD8RD8�RD9RD9�RD:RD:�RD;RD;�RD<RD<�RD=RD=�RD>RD>�RD?RD?�RD@RD@�RDARDA�RDBRDB�RDCRDC�RDDRDD�RDERDE�RDFRDF�RDGRDG�RDHRDH�RDIRDI�RDJRDJ�RDKRDK�RDLRDL�RDMRDM�RDNRDN�RDORDO�RDPRDP�RDQRDQ�RDRRDR�RDSRDS�RDTRDT�RDURDU�RDVRDV�RDWRDW�RDXRDX�RDYRDY�RDZRDZ�RD[RD[�RD\RD\�RD]RD]�RD^RD^�RD_RD_�RD`RD`�RDaRDa�RDbRDb�RDcRDc�RDdRDd�RDeRDe�RDfRDf�RDgRDg�RDhRDh�RDiRDi�RDjRDj�RDkRDk�RDlRDl�RDmRDm�RDnRDn�RDoRDo�RDpRDp�RDqRDq�RDrRDr�RDsRDs�RDtRDt�RDt�Dy�D�\D�P�D���D��)D�)D�J�D�w\D��)D�
�D�J�D���D��)D� �D�D)D�z�D�ʏD���D�0�D��D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�9XA�"�AЁA�`BA�VÃA�l�A�^5A�\)A�I�A�C�A�=qA�/A��A���A��;A��A˸RA˗�A�jA�/AʮAʙ�A�VA��Aɴ9A�dZAȥ�A�M�A�+Aŗ�A��A��A�=qA�%A��A���A�O�A��PA��HA�M�A��A��^A��A���A�+A��FA�r�A���A�O�A���A���A�n�A� �A��yA��\A��\A���A���A�ZA�A�A���A�(�A��A�?}A��uA�(�A�ƨA�Q�A��^A�1'A�v�A�v�A���A���A�A�A�ZA���A�5?A��A�G�A�
=A���A�K�A�`BA�ZA���A��TA�1A�z�A�9XA��A���A���A��A���A��uA�VA��A��-A��A�K�A���A�C�A�;A}�AzĜAw`BAq�hAj��Ai/Ag\)Adn�AbĜA`�`A^�9A[��AZ��AX�\AT��AQ`BAOt�AM�TAJA�AHz�AG�#AGVAE��AC��ABJA?A<��A;
=A:�A:bA9%A7A6E�A5�PA5"�A4��A3;dA1VA0 �A/C�A-ƨA,�HA*��A)�A'l�A'��A'XA&ȴA&5?A%XA$z�A#ƨA#dZA"ffA!�hA�A
=A^5A�-AjAO�A�A?}A�+AoA�A��A�7A|�A�A�A�A��A�A��A�mAhsA��A�+A�TAS�A�!A�A
��A
�A
{A	hsA�A��A�A&�A��A^5A��AXAA�A\)A 9X@�
=@��#@�ƨ@��#@�Q�@���@�G�@���@�Z@��@�1@�  @���@��@��m@��
@�"�@�j@�-@�hs@�/@��/@�@���@�ȴ@��@�G�@�`B@�+@�^5@��T@�w@��@�-@�h@�%@�Ĝ@��@�b@�hs@܃@�~�@�33@�{@�@�G�@��`@�1@�ƨ@�+@�E�@���@��@ЋD@��/@϶F@��@�E�@��@ə�@�$�@ʧ�@�G�@ǍP@�{@�|�@�@�|�@�dZ@�+@°!@�v�@�n�@�E�@��@��7@��@�z�@��m@��!@�@�@�p�@�&�@��@��@��w@�dZ@�o@�+@�
=@��@��@�/@�%@��`@�Ĝ@�Z@�9X@�Ĝ@��u@���@��/@�z�@�b@�t�@�+@�"�@��y@��R@���@�O�@���@�1@��m@��m@�t�@���@��!@��y@�ff@���@�I�@��R@���@���@���@�Ĝ@�r�@��@��@�V@��@���@���@��F@���@���@��@�C�@��@���@��\@�ff@���@��@���@���@� �@��m@��P@�dZ@�o@�v�@�{@��^@�7L@�%@���@��/@��@�Z@�1'@���@���@�o@��\@�^5@�=q@�$�@���@�`B@�&�@���@��9@���@��w@�o@���@���@�M�@�-@�-@�-@��#@��h@�/@���@��/@���@��u@�j@�Q�@�A�@��@��w@��@���@�C�@��@�@���@���@�n�@�-@��T@�@���@��7@�?}@���@���@��@�Z@�1@��@��P@�S�@���@�ȴ@��!@���@�^5@�-@��#@�x�@�`B@�O�@�?}@�/@�%@��/@��u@�9X@�(�@�  @��w@���@��@�;d@�"�@�@��+@�^5@�=q@�=q@�5?@�5?@�$�@�@��@���@�`B@�?}@��`@��u@��@��@���@�|�@�dZ@��@�dZ@��@��@��@�
=@�M�@�-@�@�7L@��@��@�+@��@��+@�E�@�E�@�V@�~�@�v�@�=q@���@���@��h@�\)@�ff@{"�@pA�@fȴ@^�@W��@S33@J�!@D��@>��@5@.V@)��@$9X@!%@O�@G�@�@��@/111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�9XA�"�AЁA�`BA�VÃA�l�A�^5A�\)A�I�A�C�A�=qA�/A��A���A��;A��A˸RA˗�A�jA�/AʮAʙ�A�VA��Aɴ9A�dZAȥ�A�M�A�+Aŗ�A��A��A�=qA�%A��A���A�O�A��PA��HA�M�A��A��^A��A���A�+A��FA�r�A���A�O�A���A���A�n�A� �A��yA��\A��\A���A���A�ZA�A�A���A�(�A��A�?}A��uA�(�A�ƨA�Q�A��^A�1'A�v�A�v�A���A���A�A�A�ZA���A�5?A��A�G�A�
=A���A�K�A�`BA�ZA���A��TA�1A�z�A�9XA��A���A���A��A���A��uA�VA��A��-A��A�K�A���A�C�A�;A}�AzĜAw`BAq�hAj��Ai/Ag\)Adn�AbĜA`�`A^�9A[��AZ��AX�\AT��AQ`BAOt�AM�TAJA�AHz�AG�#AGVAE��AC��ABJA?A<��A;
=A:�A:bA9%A7A6E�A5�PA5"�A4��A3;dA1VA0 �A/C�A-ƨA,�HA*��A)�A'l�A'��A'XA&ȴA&5?A%XA$z�A#ƨA#dZA"ffA!�hA�A
=A^5A�-AjAO�A�A?}A�+AoA�A��A�7A|�A�A�A�A��A�A��A�mAhsA��A�+A�TAS�A�!A�A
��A
�A
{A	hsA�A��A�A&�A��A^5A��AXAA�A\)A 9X@�
=@��#@�ƨ@��#@�Q�@���@�G�@���@�Z@��@�1@�  @���@��@��m@��
@�"�@�j@�-@�hs@�/@��/@�@���@�ȴ@��@�G�@�`B@�+@�^5@��T@�w@��@�-@�h@�%@�Ĝ@��@�b@�hs@܃@�~�@�33@�{@�@�G�@��`@�1@�ƨ@�+@�E�@���@��@ЋD@��/@϶F@��@�E�@��@ə�@�$�@ʧ�@�G�@ǍP@�{@�|�@�@�|�@�dZ@�+@°!@�v�@�n�@�E�@��@��7@��@�z�@��m@��!@�@�@�p�@�&�@��@��@��w@�dZ@�o@�+@�
=@��@��@�/@�%@��`@�Ĝ@�Z@�9X@�Ĝ@��u@���@��/@�z�@�b@�t�@�+@�"�@��y@��R@���@�O�@���@�1@��m@��m@�t�@���@��!@��y@�ff@���@�I�@��R@���@���@���@�Ĝ@�r�@��@��@�V@��@���@���@��F@���@���@��@�C�@��@���@��\@�ff@���@��@���@���@� �@��m@��P@�dZ@�o@�v�@�{@��^@�7L@�%@���@��/@��@�Z@�1'@���@���@�o@��\@�^5@�=q@�$�@���@�`B@�&�@���@��9@���@��w@�o@���@���@�M�@�-@�-@�-@��#@��h@�/@���@��/@���@��u@�j@�Q�@�A�@��@��w@��@���@�C�@��@�@���@���@�n�@�-@��T@�@���@��7@�?}@���@���@��@�Z@�1@��@��P@�S�@���@�ȴ@��!@���@�^5@�-@��#@�x�@�`B@�O�@�?}@�/@�%@��/@��u@�9X@�(�@�  @��w@���@��@�;d@�"�@�@��+@�^5@�=q@�=q@�5?@�5?@�$�@�@��@���@�`B@�?}@��`@��u@��@��@���@�|�@�dZ@��@�dZ@��@��@��@�
=@�M�@�-@�@�7L@��@��@�+@��@��+@�E�@�E�@�V@�~�@�v�@�=q@���@���G�O�@�\)@�ff@{"�@pA�@fȴ@^�@W��@S33@J�!@D��@>��@5@.V@)��@$9X@!%@O�@G�@�@��@/111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
�sB
��B�B/B1'B.B-B,B-B-B0!B1'B1'B0!B/B/B0!B.B,B'�B%�B/BF�Bp�B�%B�VB��B�B�fBJB�BuBD�BXB\)B}�B�BgmBgmBt�B�bB�{B��B��B��B�?B�}BɺB��B�FB�JB?}B-B/B2-BC�BR�BVBbNB��B��B�+B�BffBF�B.B�BbBB\B�B�B0!BB�B9XB)�B!�B�B��B�;B��B�wB�^B�9B��B�PBk�BS�BE�B9XB�B{BoB1B
��B
��B
�TB
�qB
�JB
�VB
��B
�+B
x�B
dZB
G�B
&�B
JB	�B	�5B	�XB	�\B	~�B	p�B	^5B	R�B	E�B	7LB	&�B	�B	VB��B�B�BB�B��B��BȴBĜB�wB�RB�3B�B��B��B��B��B��B��B�bB�PB�DB�7B�%B�B�B� B{�Bx�Bz�By�Bx�B�+B�\B�bB�bB�hB�uB�uB�oB�uB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�9B�LB�XB�qB��B�}B�qB�LB�RB�?B�'B�B�'B�LB�3B�'B�B�B�B��B��B�B�B�B�B�B�B��B��B�B�B�B�!B�'B�'B�'B�-B�-B�-B�!B�!B�'B�-B�-B�9B�9B�FB�FB�LB�LB�FB�XB�^B�jB�dB�wB�}B�}B�}B�}B��B�}B��B�wB�dB�jB�jB�jB�jB�jB�wB�wB�wBÖBȴBɺB��B��BɺBBŢB��B��B�5B�)B�B�B��B��B�;B�HB�HB�NB�ZB�ZB�ZB�`B�`B�`B�ZB�`B�sB�B�B�B��B��B	  B��B	  B	B	B		7B	
=B	PB	oB	uB	�B	�B	�B	�B	$�B	&�B	+B	0!B	5?B	8RB	;dB	<jB	=qB	>wB	?}B	?}B	?}B	B�B	E�B	F�B	F�B	E�B	H�B	M�B	P�B	S�B	R�B	VB	W
B	W
B	XB	XB	[#B	\)B	\)B	^5B	gmB	l�B	n�B	s�B	w�B	y�B	z�B	{�B	� B	�+B	�7B	�7B	�DB	�VB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�!B	�!B	�!B	�!B	�3B	�?B	�LB	�LB	�LB	�LB	�RB	�XB	�^B	�jB	�qB	�wB	�wB	�wB	�wB	�wB	�}B	��B	��B	��B	��B	ĜB	ŢB	ŢB	ƨB	ǮB	ǮB	ǮB	ɺB	ɺB	��B	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�#B	�)B	�)B	�)B	�)B	�)B	�)B	�5B	�5B	�;B	�;B	�;B	�;B	�BB	�NB	�NB	�NB	�NB	�NB	�TB	�TB	�ZB	�`B	�fB	�fB	�fB	�fB	�mB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
1B
bB
�B
#�B
+B
0!B
5?B
=qB
A�B
F�B
M�B
T�B
YB
_;B
bNB
dZB
gmB
k�B
p�B
t�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
�jB
��B�B/B1B.B-B, B-B-B0B1B1B0B/B/B0B.
B+�B'�B%�B/BF�Bp�B�B�LB�|B�B�\B=BBlBD�BXB\B}�B�BgbBgaBt�B�YB�qB��B��B��B�4B�rBɯB��B�=B�?B?rB-B/B2 BC�BR�BU�BbDB��B��B�B�Bf[BF�B.B�BUBBQB�B�B0BB�B9LB)�B!�B�B��B�/BʳB�hB�TB�-B��B�CBk{BS�BE�B9LB�BqBdB$B
��B
��B
�MB
�iB
�AB
�LB
��B
�"B
x�B
dNB
G�B
&�B
EB	�B	�0B	�RB	�ZB	~�B	p�B	^7B	R�B	E�B	7NB	&�B	�B	XB��B�B�GB�B��B��BȹBĤB�~B�ZB�7B�	B��B��B��B��B��B��B�kB�XB�LB�?B�,B�!B�B�
B{�Bx�Bz�By�Bx�B�2B�bB�jB�hB�nB�{B�}B�uB�|B�|B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�>B�PB�ZB�tB��B�B�tB�OB�VB�BB�+B�B�*B�OB�5B�,B�B�B�B�B� B�B�B�B�B�B�
B��B�B�B�B�B�%B�(B�'B�,B�0B�/B�0B�#B�#B�)B�/B�0B�<B�<B�KB�IB�NB�MB�JB�WB�aB�kB�eB�xB�B�}B�B�B��B�~B��B�yB�fB�mB�lB�mB�lB�nB�xB�yB�zBØBȷBɻB��B��BɼBBŠB��B��B�5B�'B�B�B��B��B�9B�FB�IB�OB�ZB�XB�YB�]B�^B�^B�ZB�^B�sB�B�B�B��B��B��B��B��B	B	B		5B	
9B	OB	lB	sB	�B	�B	�B	�B	$�B	&�B	*�B	0B	5;B	8NB	;aB	<eB	=lB	>rB	?zB	?yB	?xB	B�B	E�B	F�B	F�B	E�B	H�B	M�B	P�B	S�B	R�B	U�B	WB	WB	XB	X
B	[B	\$B	\#B	^/B	gfB	l�B	n�B	s�B	w�B	y�B	z�B	{�B	�B	�#B	�0B	�0B	�?B	�OB	�mB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�+B	�7B	�DB	�EB	�DB	�EB	�JB	�PB	�WB	�bB	�fB	�nB	�nB	�pB	�oB	�qB	�vB	�zB	�{B	�zB	��B	đB	ŘB	ŘB	ƠB	ǤB	ǤB	ǤB	ɳB	ɴB	ʹB	ɰB	ʹB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	� B	�B	�B	� B	�B	�,B	�,B	�3B	�4B	�1B	�1B	�7B	�BB	�DB	�DB	�EB	�DB	�JB	�KB	�OB	�XB	�]B	�]B	�[B	�[B	�dB	�jB	�jB	�pB	�tB	�|B	�zB	�{B	�|B	�tB	�|B	�vB	�tB	�zB	�|B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��G�O�B	��B
&B
XB
�B
#�B
*�B
0B
54B
=eB
A~B
F�B
M�B
T�B
Y
B
_/B
bEB
dLB
gaB
kxB
p�B
t�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.13 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436472016080714364720160807143647  AO  ARCAADJP                                                                    20160606191552    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160606191552  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160606191552  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143647  IP                  G�O�G�O�G�O�                