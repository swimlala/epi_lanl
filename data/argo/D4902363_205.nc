CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-02-01T00:35:43Z creation;2018-02-01T00:35:49Z conversion to V3.1;2019-12-19T07:50:27Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7T   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     88   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8X   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8\   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8d   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8h   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8p   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8x   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p4   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �4   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �4   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �d   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �t   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �x   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �|   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180201003543  20200115121518  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0576_205                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�H�%�~ 1   @�H��s��@:�������dY��#��1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @9��@�  @�  A��A   A@  A`  A�  A�  A�  A�33A�33A�  A�  A�  B   B  B  B  B   B(ffB0  B8  B@  BH  BO��BX  B`  Bh  Bo��Bw��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Ce�fCh  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՃ3D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @A�@�(�@�(�A�A"{AB{Ab{A�
=A�
=A�
=A�=qA�=qA�
=A�
=A�
=B �B�B�B�B �B(�B0�B8�B@�BH�BP�BX�B`�Bh�Bp�Bx�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�\B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C !HC!HC!HC!HC!HC
!HC!HC!HC!HC!HC!HC!HC!HC!HC!HC!HC !HC"!HC$!HC&!HC(!HC*!HC,!HC.!HC0!HC2!HC4!HC6!HC8!HC:!HC<!HC>!HC@!HCB!HCD!HCF!HCH!HCJ!HCL!HCN!HCP!HCR!HCT!HCV!HCX!HCZ!HC\!HC^!HC`!HCb!HCd!HCf�Ch!HCj!HCl!HCn!HCp!HCr!HCt!HCv!HCx!HCz!HC|!HC~!HC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D RD �RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RD	RD	�RD
RD
�RDRD��DRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RD RD �RD!RD!�RD"RD"�RD#RD#�RD$RD$�RD%RD%�RD&RD&�RD'RD'�RD(RD(�RD)RD)�RD*RD*�RD+RD+�RD,RD,�RD-RD-�RD.RD.�RD/RD/�RD0RD0�RD1RD1�RD2RD2�RD3RD3�RD4RD4�RD5RD5�RD6RD6�RD7RD7�RD8RD8�RD9RD9�RD:RD:�RD;RD;�RD<RD<�RD=RD=�RD>RD>�RD?RD?�RD@RD@�RDARDA�RDBRDB�RDCRDC�RDDRDD�RDERDE�RDFRDF�RDGRDG�RDHRDH�RDIRDI�RDJRDJ�RDKRDK�RDLRDL�RDMRDM�RDNRDN�RDORDO�RDPRDP�RDQRDQ�RDRRDR�RDSRDS�RDTRDT�RDURDU�RDVRDV�RDWRDW�RDXRDX�RDYRDY�RDZRDZ�RD[RD[�RD\RD\�RD]RD]�RD^RD^�RD_RD_�RD`RD`�RDaRDa�RDbRDb�RDcRDc�RDdRDd�RDeRDe�RDfRDf�RDgRDg�RDhRDh�RDiRDi�RDjRDj�RDkRDk�RDlRDl�RDmRDm�RDnRDn�RDoRDo�RDpRDp�RDqRDq�RDrRDr�RDsRDs�RDtRDt�RDuRDu�RDvRDv�RDwRDw�RDxRDx�RDyRDy�RDzRDz�RD{RD{�RD|RD|�RD}RD}�RD~RD~�RDRD�RD�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�\D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D)D��)D�)D�D)DÄ)D��)D�)D�D)DĄ)D��)D�)D�D)Dń)D��)D�)D�D)DƄ)D��)D�)D�D)DǄ)D��)D�)D�D)DȄ)D��)D�)D�D)DɄ)D��)D�)D�D)Dʄ)D��)D�)D�D)D˄)D��)D�)D�D)D̄)D��)D�)D�D)D̈́)D��)D�)D�D)D΄)D��)D�)D�D)Dτ)D��)D�)D�D)DЄ)D��)D�)D�D)Dф)D��)D�)D�D)D҄)D��)D�)D�D)Dӄ)D��)D�)D�D)DԄ)D��)D�)D�D)DՇ\D��)D�)D�D)Dք)D��)D�)D�D)Dׄ)D��)D�)D�D)D؄)D��)D�)D�D)Dل)D��)D�)D�D)Dڄ)D��)D�)D�D)Dۄ)D��)D�)D�D)D܄)D��)D�)D�D)D݄)D��)D�)D�D)Dބ)D��)D�)D�D)D߄)D��)D�)D�D)D��)D��)D�)D�D)D�)D��)D�)D�D)D�)D��)D�)D�D)D�)D��)D�)D�D)D�)D��)D�)D�D)D�)D��)D�)D�D)D�)D��)D�)D�D)D�)D��)D�)D�D)D�)D��)D�)D�D)D�)D��)D�)D�D)D�)D��)D�)D�D)D�)D��)D�)D�D)D�)D��)D�)D�D)D�)D��)D�)D�D)D�)D��)D�)D�D)D�)D��)D�)D�D)D��)D��)D�)D�D)D�)D��)D�)D�D)D�)D��)D�)D�D)D�)D��)D�)D�D)D�)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�G\D��)11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��uA��uA���A��DA��A�p�A�^5A�S�A�E�A�?}A�9XA�;dA�-A�$�A�$�A��A�1A�(�A��A��A��#A��
A�ȴA�ĜA�t�A�VA�%A�A��A��uA�M�A���A�5?A��A�^5A���A�  A�A��A�I�A��wA�VA��
A�=qA�O�A�-A��A��`A�v�A�E�A�jA�I�A�p�A��A���A���A�ĜA�I�A��yA�jA���A�x�A�\)A�=qA��A���A��DA��^A��!A�7LA�l�A�{A�ĜA��A�bA�XA���A�A�|�A�;dA�^A+A~�HA}��A{%Av�HAtv�As;dAr  Aq"�Ap��ApJAo
=Am�;Al��Ak\)Ai��Ah��Agt�Ag"�Af��Af�!Af��Af��Af�\Af-Ae��Ad�uAc��Aa�-A`~�A]�FAZ��AYoAXffAWx�AVZAU�AUl�AUG�AT�HAT��AT1'ARE�APVAOhsAN�`ANbAL��AJ�HAH�\AHJAG�hAF�AE�ADE�AC\)AB�`ABVAA�-AA;dA@��A@��A?��A?�A?C�A>��A=`BA;��A9��A9�A81'A7�A7�A7hsA6-A4��A41A3�^A3/A1��A0bNA/��A/�7A.ȴA.n�A-��A-�A,-A+��A+
=A*�A)ƨA(��A($�A'�TA&ĜA%�PA$��A$VA#��A#l�A"bNA!�A ��A ��A ĜA ��A v�A��A�PAv�A�PAȴA �A��A�A��A"�AjA��Av�A��A�wA�A�A"�A~�A �A�PAVA�AI�A�A;dAZA7LA��Av�A1A�mA��A
-A	|�A	oAVA��A�A�jAjA��A
=AĜA��A��A�+Az�AI�Ax�A�hA (�@�`B@�@�Ĝ@�P@��`@�=q@��@���@�F@��@�33@��#@߾w@۝�@�?}@� �@��@ם�@�;d@֟�@ԣ�@�33@�1@�33@�@���@·+@���@�x�@�X@�V@̓u@��@�J@�b@Ł@���@��@���@��@��w@���@���@�(�@��@�%@��y@��@�G�@�Ĝ@��@�V@�r�@��@���@��P@��#@�;d@�V@��-@���@�I�@��@�ƨ@��@��@���@���@��h@��@�O�@�7L@��@��@��@���@�ff@��@�?}@��/@��@���@�/@���@�Q�@���@��@�J@�@���@���@��@���@��u@�1@��F@���@�t�@�S�@�
=@���@��!@��@���@�b@�Z@���@�%@�G�@�X@�V@��/@���@�9X@�ƨ@�\)@�\)@�\)@�;d@�o@��@���@���@�x�@�V@���@�j@�1'@�b@��;@���@�\)@�+@�@�v�@�G�@���@��@�Q�@�9X@�1@���@��@���@�dZ@��@��H@�ȴ@�5?@�`B@�V@���@�r�@�(�@� �@� �@�1@��m@��@�"�@�ȴ@�v�@��@��T@�@��-@�p�@�/@���@���@�j@��
@��H@��@��u@�r�@�j@�I�@�A�@�1@~v�@}�-@}�h@}�@}�-@|�/@{dZ@{@z^5@y��@x�`@x�`@x�`@xĜ@x�@xr�@xbN@xb@w��@w��@wl�@w+@w�@w�@w�@w�@w�@w
=@w
=@v�y@v�+@vff@u�T@tz�@s��@sdZ@s"�@r�@r�!@r=q@q�@q��@q��@qx�@pQ�@o��@o|�@o;d@n��@n�R@n��@nV@n@m�h@m`B@m?}@m�@l�/@l�@l�j@k�
@k��@k�@kt�@kS�@k@j��@jM�@j=q@i��@i&�@i%@i%@i�@i�@ihs@i��@i��@i��@i��@ix�@ix�@ix�@i7L@f��@fV@fV@fE�@fE�@f$�@f$�@fE�@fv�@f$�@d�j@dI�@d9X@d1@c��@c��@dz�@b�@b^5@bJ@aX@`�@_�w@_�P@_l�@_�@^v�@^E�@^$�@]�@]O�@]?}@\��@\1@[ƨ@[��@[��@[��@[��@[��@[��@[��@[@Zn�@ZM�@Z�@Y�@Yx�@Y&�@X�@W�P@V�@V�+@Vff@VV@VE�@VV@VE�@V5?@V$�@V@UO�@T�/@T��@T�j@T�@TZ@S��@S�@SC�@R^5@Q��@Q�^@Q��@QX@P�9@P1'@O�;@O�@PQ�@Ol�@N5?@M��@M�-@M��@M�@Mp�@MO�@M/@M/@M?}@L��@L�j@Lz�@LZ@L(�@K�@K33@J�@J^5@I��@I�#@I�^@I��@IX@I�@H��@HQ�@G��@G�@F�y@F�y@F�y@F�y@Fȴ@F��@F��@Fv�@E��@E�@EO�@E�@DI�@CdZ@CC�@C"�@C@B�@B��@B�\@B^5@BJ@A��@A��@AG�@A%@@Ĝ@@bN@@ �@@  @?�;@?�@?|�@?+@>��@>5?@=�@=�-@<��@<(�@;�m@;��@;"�@:=q@9�#@9�^@9��@9x�@9G�@97L@9�@8Ĝ@8r�@7�@7�@7�w@7l�@7+@6�y@6�R@6v�@6{@5�-@5�@5O�@4��@4�D@4Z@41@3��@3�@3t�@3t�@3�@3dZ@333@3"�@3o@2�@2��@2n�@2=q@2�@1��@17L@17L@17L@1%@0�9@0��@0�u@0bN@0 �@/\)@.ȴ@.v�@.v�@.V@-��@-O�@-�@,�@,Z@,9X@,1@+��@+C�@+"�@*�@*��@*~�@*n�@)��@)��@)hs@)G�@)G�@)�@)%@)%@(�`@(��@(�u@(r�@(bN@(A�@(1'@(  @'�@'��@'�w@'�P@'�@&ȴ@&ff@&$�@%�@%�h@%p�@%p�@%`B@%O�@%/@$��@$��@$Z@#��@#�m@#�
@#�@"�H@"�!@"^5@!��@!��@!X@!&�@!%@ �`@ �9@ ��@ �u@ �@ Q�@ b@��@�@��@5?@@�T@�T@��@�-@��@�h@�@�@`B@�@z�@z�@j@Z@�@1@�m@ƨ@t�@o@�H@��@�!@~�@M�@�@�@�^@��@x�@%@��@�u@bN@A�@ �@ �@b@b@b@b@b@  @�@|�@;d@�y@��@E�@�@�h@O�@/@��@��@j@I�@(�@1@�m@��@dZ@33@o@�@��@��@n�@=q@J@��@�7@7L@�@��@�9@��@�u@�@r�@Q�@b@�w@�P@�P@�P@;d@+@
=@��@{@�@@��@�h@�h@`B@?}@�@�@�/@��@��@�j@�@z�@(�@o@
�\@
^5@
-@
J@	�@	�#@	��@	��@	��@	��@	�^@	�7@	hs@	hs@	X@	X@	G�@	G�@	G�@	G�@	&�@�9@A�@ �@�@�;@�w@�P@\)@+@�@�y@��@v�@V@E�@5?@5?@$�@$�@�@��@�-@�@O�@�@�@��@��@�D@z�@Z@9X@(�@(�@(�@1@�
@��@t�@dZ@S�@"�@o@�H@��@��@�\@n�@-@-@�@�@�^@��@hs@G�@7L@&�@%@ �u@ Q�@ Q�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��uA��uA���A��DA��A�p�A�^5A�S�A�E�A�?}A�9XA�;dA�-A�$�A�$�A��A�1A�(�A��A��A��#A��
A�ȴA�ĜA�t�A�VA�%A�A��A��uA�M�A���A�5?A��A�^5A���A�  A�A��A�I�A��wA�VA��
A�=qA�O�A�-A��A��`A�v�A�E�A�jA�I�A�p�A��A���A���A�ĜA�I�A��yA�jA���A�x�A�\)A�=qA��A���A��DA��^A��!A�7LA�l�A�{A�ĜA��A�bA�XA���A�A�|�A�;dA�^A+A~�HA}��A{%Av�HAtv�As;dAr  Aq"�Ap��ApJAo
=Am�;Al��Ak\)Ai��Ah��Agt�Ag"�Af��Af�!Af��Af��Af�\Af-Ae��Ad�uAc��Aa�-A`~�A]�FAZ��AYoAXffAWx�AVZAU�AUl�AUG�AT�HAT��AT1'ARE�APVAOhsAN�`ANbAL��AJ�HAH�\AHJAG�hAF�AE�ADE�AC\)AB�`ABVAA�-AA;dA@��A@��A?��A?�A?C�A>��A=`BA;��A9��A9�A81'A7�A7�A7hsA6-A4��A41A3�^A3/A1��A0bNA/��A/�7A.ȴA.n�A-��A-�A,-A+��A+
=A*�A)ƨA(��A($�A'�TA&ĜA%�PA$��A$VA#��A#l�A"bNA!�A ��A ��A ĜA ��A v�A��A�PAv�A�PAȴA �A��A�A��A"�AjA��Av�A��A�wA�A�A"�A~�A �A�PAVA�AI�A�A;dAZA7LA��Av�A1A�mA��A
-A	|�A	oAVA��A�A�jAjA��A
=AĜA��A��A�+Az�AI�Ax�A�hA (�@�`B@�@�Ĝ@�P@��`@�=q@��@���@�F@��@�33@��#@߾w@۝�@�?}@� �@��@ם�@�;d@֟�@ԣ�@�33@�1@�33@�@���@·+@���@�x�@�X@�V@̓u@��@�J@�b@Ł@���@��@���@��@��w@���@���@�(�@��@�%@��y@��@�G�@�Ĝ@��@�V@�r�@��@���@��P@��#@�;d@�V@��-@���@�I�@��@�ƨ@��@��@���@���@��h@��@�O�@�7L@��@��@��@���@�ff@��@�?}@��/@��@���@�/@���@�Q�@���@��@�J@�@���@���@��@���@��u@�1@��F@���@�t�@�S�@�
=@���@��!@��@���@�b@�Z@���@�%@�G�@�X@�V@��/@���@�9X@�ƨ@�\)@�\)@�\)@�;d@�o@��@���@���@�x�@�V@���@�j@�1'@�b@��;@���@�\)@�+@�@�v�@�G�@���@��@�Q�@�9X@�1@���@��@���@�dZ@��@��H@�ȴ@�5?@�`B@�V@���@�r�@�(�@� �@� �@�1@��m@��@�"�@�ȴ@�v�@��@��T@�@��-@�p�@�/@���@���@�j@��
@��H@��@��u@�r�@�j@�I�@�A�@�1@~v�@}�-@}�h@}�@}�-@|�/@{dZ@{@z^5@y��@x�`@x�`@x�`@xĜ@x�@xr�@xbN@xb@w��@w��@wl�@w+@w�@w�@w�@w�@w�@w
=@w
=@v�y@v�+@vff@u�T@tz�@s��@sdZ@s"�@r�@r�!@r=q@q�@q��@q��@qx�@pQ�@o��@o|�@o;d@n��@n�R@n��@nV@n@m�h@m`B@m?}@m�@l�/@l�@l�j@k�
@k��@k�@kt�@kS�@k@j��@jM�@j=q@i��@i&�@i%@i%@i�@i�@ihs@i��@i��@i��@i��@ix�@ix�@ix�@i7L@f��@fV@fV@fE�@fE�@f$�@f$�@fE�@fv�@f$�@d�j@dI�@d9X@d1@c��@c��@dz�@b�@b^5@bJ@aX@`�@_�w@_�P@_l�@_�@^v�@^E�@^$�@]�@]O�@]?}@\��@\1@[ƨ@[��@[��@[��@[��@[��@[��@[��@[@Zn�@ZM�@Z�@Y�@Yx�@Y&�@X�@W�P@V�@V�+@Vff@VV@VE�@VV@VE�@V5?@V$�@V@UO�@T�/@T��@T�j@T�@TZ@S��@S�@SC�@R^5@Q��@Q�^@Q��@QX@P�9@P1'@O�;@O�@PQ�@Ol�@N5?@M��@M�-@M��@M�@Mp�@MO�@M/@M/@M?}@L��@L�j@Lz�@LZ@L(�@K�@K33@J�@J^5@I��@I�#@I�^@I��@IX@I�@H��@HQ�@G��@G�@F�y@F�y@F�y@F�y@Fȴ@F��@F��@Fv�@E��@E�@EO�@E�@DI�@CdZ@CC�@C"�@C@B�@B��@B�\@B^5@BJ@A��@A��@AG�@A%@@Ĝ@@bN@@ �@@  @?�;@?�@?|�@?+@>��@>5?@=�@=�-@<��@<(�@;�m@;��@;"�@:=q@9�#@9�^@9��@9x�@9G�@97L@9�@8Ĝ@8r�@7�@7�@7�w@7l�@7+@6�y@6�R@6v�@6{@5�-@5�@5O�@4��@4�D@4Z@41@3��@3�@3t�@3t�@3�@3dZ@333@3"�@3o@2�@2��@2n�@2=q@2�@1��@17L@17L@17L@1%@0�9@0��@0�u@0bN@0 �@/\)@.ȴ@.v�@.v�@.V@-��@-O�@-�@,�@,Z@,9X@,1@+��@+C�@+"�@*�@*��@*~�@*n�@)��@)��@)hs@)G�@)G�@)�@)%@)%@(�`@(��@(�u@(r�@(bN@(A�@(1'@(  @'�@'��@'�w@'�P@'�@&ȴ@&ff@&$�@%�@%�h@%p�@%p�@%`B@%O�@%/@$��@$��@$Z@#��@#�m@#�
@#�@"�H@"�!@"^5@!��@!��@!X@!&�@!%@ �`@ �9@ ��@ �u@ �@ Q�@ b@��@�@��@5?@@�T@�T@��@�-@��@�h@�@�@`B@�@z�@z�@j@Z@�@1@�m@ƨ@t�@o@�H@��@�!@~�@M�@�@�@�^@��@x�@%@��@�u@bN@A�@ �@ �@b@b@b@b@b@  @�@|�@;d@�y@��@E�@�@�h@O�@/@��@��@j@I�@(�@1@�m@��@dZ@33@o@�@��@��@n�@=q@J@��@�7@7L@�@��@�9@��@�u@�@r�@Q�@b@�w@�P@�P@�P@;d@+@
=@��@{@�@@��@�h@�h@`B@?}@�@�@�/@��@��@�j@�@z�@(�@o@
�\@
^5@
-@
J@	�@	�#@	��@	��@	��@	��@	�^@	�7@	hs@	hs@	X@	X@	G�@	G�@	G�@	G�@	&�@�9@A�@ �@�@�;@�w@�P@\)@+@�@�y@��@v�@V@E�@5?@5?@$�@$�@�@��@�-@�@O�@�@�@��@��@�D@z�@Z@9X@(�@(�@(�@1@�
@��@t�@dZ@S�@"�@o@�H@��@��@�\@n�@-@-@�@�@�^@��@hs@G�@7L@&�@%@ �u@ Q�@ Q�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B\B�BuBoBPB1BoBoBbB+BB�B�B��B��B��B�\B�DB�7B|�BiyBE�B=qB�B�ZB�TB��B�}B�wB�FB��Bl�BO�B[#BYBF�B0!B33B�B%B
��B
��BDBDB+BB
��B
�mB
��B
�B
��B
��B
ȴB
�RB
�B
��B
��B
�{B
�DB
�JB
�B
}�B
v�B
bNB
>wB
"�B
 �B
(�B
!�B
 �B
�B
�B
\B
B	��B	�B	�mB	�yB	�`B	�sB	�B	�B	�yB	�yB	�`B	�5B	�B	ȴB	�wB	�B	��B	��B	|�B	�B	�=B	�B	~�B	z�B	�B	�B	{�B	w�B	p�B	^5B	J�B	T�B	R�B	H�B	=qB	0!B	�B	/B	,B	"�B	�B	{B	oB	�B	hB	\B	VB	VB	JB	B	B	B��B�B�BB�B�sB�HB�mB�ZB�;B��BȴB��B��BɺB�^B�RB��B�}B�XB�dB�FB�B�B�B�B�B��B��B��B��B�uB�VB�hB�uB�bB�\B�%B�+B�1B�PB�VB�7B�Bv�B_;Bl�Bm�Bl�Bm�Bl�BiyBe`BXB^5BaHBVB]/BgmBffBcTB^5BZB[#BYBW
BYBVBQ�BO�BJ�BF�BJ�BR�BN�BM�BI�B;dB=qBD�B>wB>wB@�B@�BA�B:^B>wBB�BE�BD�BC�B@�B:^B-B�B�B �B �B�B	7B{B�B�BuB�B�BVB�BoBBoB�B#�B!�B�B�BuBoBbB �B)�B)�B(�B'�B'�B+B'�B$�B!�B�B{B�B�B'�B%�B'�B$�B%�B%�B+B(�B#�B&�B5?B:^B:^B33B;dB8RBB�BF�BB�B9XB49BI�BO�BL�BVBXBW
BVBVB`BBdZBdZBcTBcTBbNB_;BaHBcTBaHBe`BdZBbNBffBdZBaHBr�Bu�Bt�Bu�Bv�Bz�B�B�B�%B�B�B�%B�+B�7B�PB�PB�PB�JB�VB�VB�DB�DB��B�B�-B�9B�FB�FB�RB�jB��BĜBȴB��B��B��B��B��B��B��B��B��B�B�)B�/B�BB�HB�HB�HB�NB�TB�TB�NB�BB�mB�yB�B��B��B��B��B��B��B��B��B��B��B��B	  B	B	%B	JB	bB	oB	oB	uB	uB	�B	�B	 �B	"�B	)�B	,B	/B	0!B	2-B	6FB	9XB	:^B	8RB	6FB	0!B	;dB	>wB	?}B	?}B	?}B	=qB	;dB	B�B	H�B	J�B	L�B	N�B	J�B	P�B	Q�B	S�B	VB	_;B	bNB	cTB	dZB	e`B	ffB	e`B	gmB	hsB	iyB	jB	k�B	k�B	l�B	l�B	l�B	l�B	l�B	l�B	k�B	k�B	jB	iyB	p�B	v�B	v�B	x�B	x�B	y�B	|�B	}�B	~�B	}�B	{�B	�B	�B	�B	�+B	�1B	�=B	�=B	�PB	�\B	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�!B	�!B	�'B	�'B	�-B	�-B	�!B	�B	�3B	�LB	�RB	�RB	�RB	�XB	�^B	�dB	�RB	�FB	�dB	�wB	�wB	�wB	��B	ĜB	�wB	��B	ÖB	��B	B	ĜB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�B	�
B	�B	�B	�#B	�B	�B	�#B	�#B	�5B	�TB	�`B	�fB	�fB	�fB	�fB	�fB	�fB	�`B	�ZB	�sB	�B	�B	�B	�B	�yB	�B	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B	��B
B
B
B
B
+B
+B
+B
+B
+B
1B
+B
1B

=B
JB
VB
VB
VB
PB
VB
VB
PB
JB
JB
VB
VB
JB
PB
oB
oB
oB
oB
oB
oB
oB
oB
{B
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
�B
�B
 �B
#�B
$�B
#�B
#�B
#�B
$�B
$�B
$�B
%�B
&�B
&�B
&�B
'�B
)�B
)�B
+B
+B
)�B
+B
,B
,B
+B
,B
-B
.B
.B
-B
0!B
0!B
0!B
/B
0!B
0!B
0!B
/B
/B
0!B
2-B
33B
33B
2-B
33B
5?B
49B
5?B
7LB
6FB
6FB
6FB
7LB
7LB
7LB
7LB
7LB
6FB
6FB
8RB
8RB
9XB
8RB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
;dB
;dB
;dB
;dB
<jB
;dB
;dB
<jB
<jB
=qB
=qB
=qB
?}B
?}B
?}B
?}B
?}B
?}B
>wB
>wB
@�B
B�B
B�B
A�B
@�B
B�B
B�B
B�B
C�B
D�B
E�B
F�B
F�B
F�B
G�B
G�B
F�B
F�B
F�B
E�B
E�B
F�B
H�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
J�B
H�B
K�B
L�B
M�B
L�B
L�B
M�B
L�B
L�B
L�B
L�B
N�B
O�B
O�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
N�B
O�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
Q�B
P�B
Q�B
Q�B
R�B
R�B
S�B
S�B
T�B
VB
T�B
T�B
VB
W
B
W
B
W
B
W
B
W
B
W
B
XB
XB
XB
XB
XB
XB
YB
XB
YB
XB
XB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
\)B
]/B
\)B
[#B
\)B
\)B
[#B
[#B
^5B
^5B
^5B
_;B
_;B
^5B
_;B
_;B
_;B
`BB
`BB
`BB
_;B
_;B
^5B
\)B
ZB
_;B
bNB
bNB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
dZB
dZB
bNB
cTB
e`B
ffB
ffB
ffB
ffB
ffB
ffB
gmB
gmB
ffB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
hsB
hsB
iyB
hsB
iyB
iyB
jB
jB
jB
k�B
k�B
k�B
k�B
l�B
l�B
l�B
k�B
k�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
o�B
o�B
n�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
o�B
q�B
r�B
r�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ByB�B�B�ByB�B�ByByB�B�ByByB�B�B�ByB�B�B�B�B�ByB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�BeB�B�B�B�B�B�B�B�BoBoB�B�B�B��B�]B�_B�
B�'B��B��B��B~BkBH�B?}B�B�B��B�[B��B�}B�2B�OBqABSuB\)BZBH�B2aB4�B�B	lB �B
��B^BxBzB�B
�B
�_B
�SB
�=B
ϑB
��B
ɺB
�*B
�CB
�~B
�7B
��B
�~B
�B
�B
~�B
w�B
d@B
B'B
'mB
#nB
*eB
# B
!�B
 \B
yB
�B
[B	�"B	�B	�DB	�B	�B	��B	��B	�B	�B	�B	�zB	޸B	��B	�=B	� B	�oB	��B	��B	�iB	��B	�B	�?B	�OB	{�B	�AB	�UB	|jB	xRB	q�B	`vB	MB	U�B	S�B	I�B	?HB	2|B	VB	/�B	,�B	#�B	YB	�B	�B	B	:B	.B	�B	�B	�B	B	�B	�B�B�WB�B�qB�*B�4B�B��B��BӏB�rB�}B�\B��B�PB��B� B�B�*B��B��B�UB�"B��B��B��B��B��B��B�VB��B��B�oB��B�4B�B�_B��B�B�jB��B��B��Bx8Ba�Bm�Bn�Bm�BncBm]BjeBffBY�B_!BbNBW�B]�BgmBf�Bc�B^�B[	B[�BY�BW�BY�BV�BR�BP�BK�BG�BKxBS[BOvBN<BJXB=<B>wBEB?}B?cBA;BABBB;B?.BB�BE�BD�BC�B@�B:�B.�B!-B!�B"�B"hBjBdB9B1B	BgB�B�BbB�BB�B�BWB#�B"B 'B]B�B�B B!-B*B*0B)*B(XB(>B+B(XB%`B"hBBB+B�B(�B&�B(�B%�B&�B'B+�B)�B%FB(XB5�B:�B:�B4nB;�B9rBB�BF�BCB:�B5�BJrBPbBM�BV9BXEBWsBV�BV�B`vBdtBdZBcnBcnBb�B_�Ba�Bc�Ba�Be�Bd�Bb�Bf�BeFBb�Br�BvBu%Bv+BwfB{dB�-B�9B�?B�SB��B�tB�zB�lB�jB��B�jB�~B��B��B��B�B��B��B��B��B�B�FB��B��B��B��B�B�B��B��B�B�&B�B�@B�NB�gB�KB�]B�~B�\B�bB�|B�|B�hB�nB�B�B��B�B��B�B��B��B��B��B��B��B�%B��B��B�>B�XB	 B	GB	tB	~B	bB	oB	�B	�B	�B	�B	�B	!B	# B	)�B	,"B	/B	0UB	2|B	6zB	9rB	:�B	8�B	6�B	1AB	;�B	>�B	?�B	?}B	?�B	=�B	;�B	B�B	H�B	J�B	L�B	O(B	K^B	QB	R B	T,B	VSB	_!B	b4B	cTB	dtB	ezB	f�B	ezB	g�B	hsB	iyB	j�B	k�B	kkB	lqB	l�B	lqB	l�B	l�B	l�B	k�B	k�B	j�B	jB	p�B	v�B	v�B	x�B	x�B	y�B	|�B	}�B	B	~B	|PB	� B	�B	�B	�EB	�1B	�XB	�XB	�PB	��B	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�'B	�-B	�-B	�oB	��B	�MB	�LB	�RB	�RB	�RB	�>B	�DB	�dB	��B	��B	�dB	�]B	�wB	�wB	�oB	āB	��B	��B	ðB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�
B	��B	��B	�B	��B	�+B	�9B	�$B	�7B	�B	�#B	�QB	�QB	�qB	�WB	�OB	�nB	�zB	�LB	�fB	�LB	�fB	�fB	�fB	�`B	�B	�sB	�kB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B
 B
  B	�B
B
-B
-B
B
EB
EB
+B
+B
EB
1B
EB
�B

XB
JB
<B
<B
VB
jB
VB
<B
PB
~B
dB
pB
pB
~B
�B
TB
oB
�B
�B
oB
oB
oB
�B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
�B
�B
 �B
#�B
$�B
#�B
$B
#�B
$�B
$�B
$�B
%�B
'B
&�B
'B
'�B
)�B
)�B
*�B
+B
)�B
+B
+�B
,B
+B
,B
-)B
.B
.B
-)B
0!B
0B
0!B
/B
0!B
0!B
0!B
/OB
/OB
0UB
2GB
33B
33B
2aB
3MB
5?B
4nB
5?B
7LB
6`B
6`B
6FB
7LB
7fB
7fB
7LB
7LB
6zB
6zB
8RB
8RB
9>B
8RB
9>B
9>B
9XB
9XB
:^B
:^B
:DB
:xB
;dB
;dB
;dB
;B
<�B
;dB
;B
<�B
<jB
=qB
=�B
=�B
?cB
?}B
?cB
?�B
?}B
?�B
>�B
>�B
@�B
B�B
B�B
A�B
@�B
B�B
B�B
B�B
C�B
D�B
E�B
F�B
F�B
F�B
G�B
G�B
F�B
F�B
F�B
E�B
E�B
F�B
H�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
J�B
H�B
K�B
L�B
M�B
L�B
L�B
M�B
L�B
L�B
L�B
L�B
N�B
O�B
O�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
N�B
O�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
RB
Q B
Q�B
RB
SB
S&B
TB
S�B
T�B
VB
T�B
T�B
VB
W$B
W
B
W
B
W
B
W$B
W
B
XB
X+B
XB
XB
X+B
X+B
YB
X+B
YB
X+B
XB
ZB
ZB
[#B
[	B
[#B
[	B
[#B
[#B
[=B
[#B
\)B
]B
\B
[#B
\)B
\)B
[WB
[=B
^B
^OB
^OB
_;B
_!B
^5B
_;B
_VB
_VB
`BB
`'B
`BB
_VB
_;B
^5B
\CB
ZkB
_VB
bNB
bNB
cTB
cTB
dZB
d@B
dZB
dZB
dZB
dZB
dZB
dZB
eFB
eFB
e`B
e`B
eFB
eFB
d@B
dZB
b�B
cnB
ezB
ffB
ffB
f�B
ffB
ffB
ffB
gmB
gmB
f�B
hsB
hsB
i_B
iyB
i_B
i_B
i_B
hsB
hsB
iyB
h�B
i�B
i�B
jB
j�B
j�B
k�B
kkB
k�B
k�B
l�B
lqB
lqB
k�B
k�B
l�B
l�B
mwB
m�B
m�B
m�B
m�B
m�B
n�B
n}B
n�B
n�B
o�B
o�B
n�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
o�B
q�B
r�B
r�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.13(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201802050031172018020500311720180205003117201806221237062018062212370620180622123706201804050433452018040504334520180405043345  JA  ARFMdecpA19c                                                                20180201093519  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180201003543  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180201003546  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180201003546  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180201003547  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180201003547  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180201003547  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180201003547  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180201003548  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180201003549                      G�O�G�O�G�O�                JA  ARUP                                                                        20180201005640                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180201153446  CV  JULD            G�O�G�O�F�G�                JM  ARCAJMQC2.0                                                                 20180204153117  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180204153117  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404193345  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622033706  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115121518                      G�O�G�O�G�O�                