CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-06-09T14:01:05Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         C   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    9   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    9    HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    9$   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    9(   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    98   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    9H   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    9X   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  9`   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  9�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  @  9�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        :    	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    :$   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    :(   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     :,   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    :L   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    :P   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     :T   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     :t   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     :�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    :�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           :�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    :�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            :�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           :�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           :�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    :�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    :�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    :�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        ;�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    ;�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    ;�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    ;�   PROFILE_NB_SAMPLE_CTD_QC               	long_name         ,Global quality flag of NB_SAMPLE_CTD profile   conventions       Argo reference table 2a    
_FillValue                    ;�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        T  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  KP   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     T  O(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ^|   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     T  bT   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     T  q�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     T  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     T  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     T  �T   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     T  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     T  ͬ   NB_SAMPLE_CTD            
         	long_name         2Number of samples in each pressure bin for the CTD     
_FillValue        �     units         count      C_format      %5d    FORTRAN_format        I5     
resolution                �  �    NB_SAMPLE_CTD_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  @  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  8  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �    HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �L   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �\   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �`   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �p   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �t   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �x   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �|Argo profile    3.1 1.2 19500101000000  20230609140105  20230609140105  5906592 Argo PMEL                                                       GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            NB_SAMPLE_CTD      LA   AO  8756                            2B  A   NAVIS_A                         1284                            170425                          863 @�1��h�1   @�1�DDT�@0a���l��d%o hی1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         LA   A   A       @�33@�  A   A   A@  A`  A�  A�33A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf�Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�C3D�ff111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�\)@�(�A{A"{AB{Ab{A�
=A�=pA�
=A�
=A�
=A�
=A�
=A�
=B �B�B�B�B �B(�B0�B8�B@�BH�BP�BX�B`�Bh�Bp�Bx�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C !HC!HC!HC!HC!HC
!HC!HC!HC!HC!HC!HC!HC!HC!HC!HC!HC !HC"!HC$!HC&!HC(!HC*!HC,!HC.!HC0!HC2!HC4!HC6!HC8!HC:!HC<!HC>!HC@!HCB!HCD!HCF!HCH!HCJ!HCL!HCN!HCP!HCR!HCT!HCV!HCX!HCZ!HC\!HC^!HC`!HCb!HCd!HCf:�Ch!HCj!HCl!HCn!HCp!HCr!HCt!HCv!HCx!HCz!HC|!HC~!HC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D RD �RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RD	RD	�RD
RD
�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RD RD �RD!RD!�RD"RD"�RD#RD#�RD$RD$�RD%RD%�RD&RD&�RD'RD'�RD(RD(�RD)RD)�RD*RD*�RD+RD+�RD,RD,�RD-RD-�RD.RD.�RD/RD/�RD0RD0�RD1RD1�RD2RD2�RD3RD3�RD4RD4�RD5RD5�RD6RD6�RD7RD7�RD8RD8�RD9RD9�RD:RD:�RD;RD;�RD<RD<�RD=RD=�RD>RD>�RD?RD?�RD@RD@�RDARDA�RDBRDB�RDCRDC�RDDRDD�RDERDE�RDFRDF�RDGRDG�RDHRDH�RDIRDI�RDJRDJ�RDKRDK�RDLRDL�RDMRDM�RDNRDN�RDORDO�RDPRDP�RDQRDQ�RDRRDR�RDSRDS�RDTRDT�RDURDU�RDVRDV�RDWRDW�RDXRDX�RDYRDY�RDZRDZ�RD[RD[�RD\RD\�RD]RD]�RD^RD^�RD_RD_�RD`RD`�RDaRDa�RDbRDb�RDcRDc�RDdRDd�RDeRDe�RDfRDf�RDgRDg�RDhRDh�RDiRDi�RDjRDj�RDkRDk�RDlRDl�RDmRDm�RDnRDn�RDoRDo�RDpRDp�RDqRDq�RDrRDr�RDsRDs�RDtRDt�RDuRDu�RDvRDv�RDwRDw�RDxRDx�RDyRDy�RDzRDz�RD{RD{�RD|RD|�RD}RD}�RD~RD~�RDRD�RD�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�\D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D)D��)D�)D�D)DÄ)D��)D�)D�D)DĄ)D��)D�)D�D)Dń)D��)D�)D�D)DƄ)D��)D�)D�D)DǄ)D��)D�)D�D)DȄ)D��)D�)D�D)DɄ)D��)D�)D�D)Dʄ)D��)D�)D�D)D˄)D��)D�)D�D)D̄)D��)D�)D�D)D̈́)D��)D�)D�D)D΄)D��)D�)D�D)Dτ)D��)D�)D�D)DЄ)D��)D�)D�D)Dф)D��)D�)D�D)D҄)D��)D�)D�D)Dӄ)D��)D�)D�D)DԄ)D��)D�)D�D)DՄ)D��)D�)D�D)Dք)D��)D�)D�D)Dׄ)D��)D�)D�D)D؄)D��)D�)D�D)Dل)D��)D�)D�D)Dڄ)D��)D�)D�D)Dۄ)D��)D�)D�D)D܄)D��)D�)D�D)D݄)D��)D�)D�D)Dބ)D��)D�)D�D)D߄)D��)D�)D�D)D��)D��)D�)D�D)D�)D��)D�)D�D)D�)D��)D�)D�D)D�)D��)D�)D�D)D�)D��)D�)D�D)D�)D��)D�)D�D)D�)D��)D�)D�D)D�)D��)D�)D�D)D�)D��)D�)D�D)D�)D��)D�)D�D)D�)D��)D�)D�D)D�)D��)D�)D�D)D�)D��)D�)D�D)D�)D��)D�)D�D)D�)D��)D�)D�D)D�)D��)D�)D�D)D��)D��)D�)D�D)D�)D��)D�)D�D)D�)D��)D�)D�D)D�)D��)D�)D�D)D�)D��\D�)D�G\D�j�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AҮAҮAҬAҩ�Aҡ�Aҥ�Aҩ�AҮAҺ^AҼjAҼjAҾwA���A���AҾwAҺ^AҾwA�AҼjAҲ-AҶFAҰ!Aң�A�t�A�O�A�K�A�33A�+A�JA��`AѴ9A��A��`A�Aϛ�A΍PA�K�A̟�A�r�A˾wA�S�A���A�/Aɡ�A��A�ffA�E�AƮA���A�M�Aĥ�A��yA�VA�;dA\A�v�A�r�A���A��^A���A�VA�hsA�A��A���A��#A�bA�x�A�`BA��A���A�~�A�A���A�=qA���A�{A�I�A��FA�1A�S�A��A�"�A��A��A�^5A���A�ZA��A��\A��-A��!A�9XA�(�A�O�A�ffA�33A�jA�7LA���A{O�Ar~�AoƨAnJAmK�Alz�Ak��Ai��Ab��Aa��AaVA]ƨA[AZ��AY��AUO�AQ�AJ��AH�AG+AE��AD�uAA�;A@bNA?��A>v�A<�yA:�A77LA5`BA4ZA1��A0Q�A/oA-��A,��A+C�A*9XA)�
A)p�A(��A'?}A%��A%l�A%��A$�`A#K�A"ȴA#
=A"I�A!�A 1'A��A�AXA�Av�A�hA;dA�DA�uA�+Az�A��A�RA�A�!At�A��A�A��A�
A7LA�\A�A9XA�AS�AS�A��A�+AA�A�A
��A
JA	;dA�RAA�A��At�A/AȴA��A$�A��Ap�A�/A��An�AM�A�;A�hAp�AS�AC�A+A�A�!A�A5?AG�A ��A V@�l�@�@��@��^@�v�@�$�@�-@�ff@�E�@�{@�@���@�hs@� �@��@�x�@��j@�Q�@���@���@�hs@��@�O�@� �@�v�@��`@�+@�7L@���@�1@�R@�V@��@�^@���@��
@�@�P@�o@���@�ȴ@�$�@�/@��@�Ĝ@�9X@�ƨ@�dZ@��@�M�@���@�9@��
@���@���@���@�@��y@ڟ�@�^5@�ff@�-@���@؋D@�I�@� �@��
@��@��@ղ-@�7L@�I�@�  @�t�@��@�~�@�=q@��@ѡ�@�`B@�7L@��`@�bN@�t�@�n�@��@ͺ^@͉7@�p�@�/@̋D@��
@�"�@ʇ+@�J@Ɂ@�G�@��@���@Ȭ@�Z@�dZ@���@ư!@Ƨ�@ư!@ƸR@ư!@Ɨ�@Ƈ+@Ų-@�V@��/@ă@Å@�C�@�"�@�v�@�x�@��`@�9X@��F@�\)@�o@��!@�5?@�@��T@���@�X@���@��@�  @��@��y@���@���@���@�{@�/@�&�@��/@��j@�1'@��
@�ƨ@�ƨ@���@�\)@��@��R@���@��\@�v�@�-@��^@�O�@��@���@�Q�@���@�|�@�ff@��T@��-@�J@��@��@��@��`@���@��u@�Z@�1@��F@�l�@��@�M�@���@�`B@�x�@�G�@���@��@�Ĝ@�A�@���@���@���@�v�@�V@�M�@�-@���@��@���@���@�1'@�dZ@��@���@�V@�=q@��7@�O�@��@��@�1'@��@�t�@�33@���@�^5@�{@��^@�X@���@���@�r�@�I�@� �@��m@���@�l�@�
=@��!@���@��@��^@�/@�Ĝ@�I�@��
@��@���@��F@��P@�;d@���@���@�n�@�$�@��#@���@��h@�X@���@�z�@� �@���@�;d@���@��R@���@�n�@�^5@�E�@�J@���@�?}@�&�@�&�@�V@��@��@��;@��@�t�@�o@��@�ȴ@��!@���@��!@���@���@�v�@�{@��T@���@�`B@�&�@��`@��j@���@��u@�A�@��@��m@���@��@�dZ@�K�@���@���@���@���@�n�@�=q@��T@��^@���@�`B@�%@�Ĝ@��D@�r�@�I�@�ƨ@�|�@�\)@�;d@�@��@��!@��+@�^5@��@���@�p�@�?}@��@�%@��`@�r�@��F@�t�@�K�@�o@���@�~�@���@��-@��h@��@�/@���@��@�j@�(�@�  @��m@���@���@�S�@�K�@�C�@�"�@�o@��@���@�ff@�M�@�5?@��@��T@�@��7@�`B@�G�@�&�@���@��@��j@� �@�@�@l�@~ȴ@~v�@~E�@~@}@}�@|��@|��@|�@|9X@{��@{o@zM�@y�^@x��@x  @w+@v�+@v5?@v@u�@uV@t��@tZ@t1@so@r�\@r-@qG�@q�@p�9@o�@o��@n�y@nv�@m�-@mO�@m�@l��@k��@kdZ@k"�@k"�@k"�@ko@j�@j~�@j�@iX@g�;@f�@fff@fE�@f@e��@e�h@e/@dz�@d�@c��@cdZ@b��@b^5@a�#@ahs@a&�@a%@`�`@`Ĝ@`1'@_
=@^E�@^{@]�-@\�/@\��@\��@\��@\��@\��@\�D@\1@[��@[t�@[33@Z�@Z~�@Zn�@Z^5@ZJ@Yx�@Y�@X��@X  @W�;@W�P@Vȴ@U�h@U?}@U/@U/@U�@T�/@T��@TI�@S��@S�
@S�F@SS�@R�@R�@R��@RM�@Qhs@Q&�@Q�@Q%@P�9@PQ�@P1'@P  @O;d@N��@Nȴ@N��@Mp�@L�@LI�@K�F@K��@Kt�@KC�@K@J��@J-@I��@I��@I�7@IX@H�`@Hb@F�@F�R@F�R@F�R@F�+@E�@EV@D�/@D�j@D�@D�D@D�D@D(�@Cƨ@C"�@Bn�@BM�@BJ@A��@A��@Ahs@AX@AG�@A�@@�`@@�@@bN@@A�@@1'@@b@?+@>��@>E�@>@=��@=�-@=�@=�@=V@<�@<�D@<1@;dZ@:�@:��@:�\@:�@9�^@9��@9�7@9hs@9�@81'@7��@7��@7|�@7\)@7
=@6�R@65?@5�@5�-@5/@4��@4Z@3�
@3��@3t�@3S�@3@2�H@2�H@2��@2�!@2�\@2�\@2~�@2=q@2-@2J@1�@1�^@1G�@1&�@0��@0�u@0r�@0 �@/�@/;d@/
=@.�y@.ȴ@.��@.��@.v�@.v�@.v�@.V@.5?@.$�@-�T@,��@,1@+ƨ@+�@+S�@+33@+@*�H@*��@*�\@*^5@*�@)�#@)��@)G�@)7L@)7L@)7L@)�@(��@(��@(Ĝ@(��@(r�@(1'@'��@'l�@'+@'
=@&��@&�@&�R@&��@&��@&V@&{@&@%�-@%�@%�@%V@$�@$��@$��@$��@$(�@#ƨ@#ƨ@#��@#dZ@#o@"��@"��@"n�@"=q@"�@"J@!��@!�7@!�@ �`@ ��@ r�@ bN@ A�@  �@�@�;@�@��@��@�P@|�@l�@K�@+@�y@�R@v�@{@�@/@�/@I�@�F@��@��@��@��@�@C�@33@"�@�H@��@^5@=q@-@�@J@�@��@��@�^@��@��@hs@��@��@��@��@�u@b@�w@��@\)@��@�@��@v�@V@$�@�@�T@�T@�T@�T@��@��@@�-@�h@p�@?}@V@�/@��@Z@1@�
@��@33@�@n�@=q@-@J@�@��@�7@x�@7L@�@�`@Q�@b111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   AҮAҮAҬAҩ�Aҡ�Aҥ�Aҩ�AҮAҺ^AҼjAҼjAҾwA���A���AҾwAҺ^AҾwA�AҼjAҲ-AҶFAҰ!Aң�A�t�A�O�A�K�A�33A�+A�JA��`AѴ9A��A��`A�Aϛ�A΍PA�K�A̟�A�r�A˾wA�S�A���A�/Aɡ�A��A�ffA�E�AƮA���A�M�Aĥ�A��yA�VA�;dA\A�v�A�r�A���A��^A���A�VA�hsA�A��A���A��#A�bA�x�A�`BA��A���A�~�A�A���A�=qA���A�{A�I�A��FA�1A�S�A��A�"�A��A��A�^5A���A�ZA��A��\A��-A��!A�9XA�(�A�O�A�ffA�33A�jA�7LA���A{O�Ar~�AoƨAnJAmK�Alz�Ak��Ai��Ab��Aa��AaVA]ƨA[AZ��AY��AUO�AQ�AJ��AH�AG+AE��AD�uAA�;A@bNA?��A>v�A<�yA:�A77LA5`BA4ZA1��A0Q�A/oA-��A,��A+C�A*9XA)�
A)p�A(��A'?}A%��A%l�A%��A$�`A#K�A"ȴA#
=A"I�A!�A 1'A��A�AXA�Av�A�hA;dA�DA�uA�+Az�A��A�RA�A�!At�A��A�A��A�
A7LA�\A�A9XA�AS�AS�A��A�+AA�A�A
��A
JA	;dA�RAA�A��At�A/AȴA��A$�A��Ap�A�/A��An�AM�A�;A�hAp�AS�AC�A+A�A�!A�A5?AG�A ��A V@�l�@�@��@��^@�v�@�$�@�-@�ff@�E�@�{@�@���@�hs@� �@��@�x�@��j@�Q�@���@���@�hs@��@�O�@� �@�v�@��`@�+@�7L@���@�1@�R@�V@��@�^@���@��
@�@�P@�o@���@�ȴ@�$�@�/@��@�Ĝ@�9X@�ƨ@�dZ@��@�M�@���@�9@��
@���@���@���@�@��y@ڟ�@�^5@�ff@�-@���@؋D@�I�@� �@��
@��@��@ղ-@�7L@�I�@�  @�t�@��@�~�@�=q@��@ѡ�@�`B@�7L@��`@�bN@�t�@�n�@��@ͺ^@͉7@�p�@�/@̋D@��
@�"�@ʇ+@�J@Ɂ@�G�@��@���@Ȭ@�Z@�dZ@���@ư!@Ƨ�@ư!@ƸR@ư!@Ɨ�@Ƈ+@Ų-@�V@��/@ă@Å@�C�@�"�@�v�@�x�@��`@�9X@��F@�\)@�o@��!@�5?@�@��T@���@�X@���@��@�  @��@��y@���@���@���@�{@�/@�&�@��/@��j@�1'@��
@�ƨ@�ƨ@���@�\)@��@��R@���@��\@�v�@�-@��^@�O�@��@���@�Q�@���@�|�@�ff@��T@��-@�J@��@��@��@��`@���@��u@�Z@�1@��F@�l�@��@�M�@���@�`B@�x�@�G�@���@��@�Ĝ@�A�@���@���@���@�v�@�V@�M�@�-@���@��@���@���@�1'@�dZ@��@���@�V@�=q@��7@�O�@��@��@�1'@��@�t�@�33@���@�^5@�{@��^@�X@���@���@�r�@�I�@� �@��m@���@�l�@�
=@��!@���@��@��^@�/@�Ĝ@�I�@��
@��@���@��F@��P@�;d@���@���@�n�@�$�@��#@���@��h@�X@���@�z�@� �@���@�;d@���@��R@���@�n�@�^5@�E�@�J@���@�?}@�&�@�&�@�V@��@��@��;@��@�t�@�o@��@�ȴ@��!@���@��!@���@���@�v�@�{@��T@���@�`B@�&�@��`@��j@���@��u@�A�@��@��m@���@��@�dZ@�K�@���@���@���@���@�n�@�=q@��T@��^@���@�`B@�%@�Ĝ@��D@�r�@�I�@�ƨ@�|�@�\)@�;d@�@��@��!@��+@�^5@��@���@�p�@�?}@��@�%@��`@�r�@��F@�t�@�K�@�o@���@�~�@���@��-@��h@��@�/@���@��@�j@�(�@�  @��m@���@���@�S�@�K�@�C�@�"�@�o@��@���@�ff@�M�@�5?@��@��T@�@��7@�`B@�G�@�&�@���@��@��j@� �@�@�@l�@~ȴ@~v�@~E�@~@}@}�@|��@|��@|�@|9X@{��@{o@zM�@y�^@x��@x  @w+@v�+@v5?@v@u�@uV@t��@tZ@t1@so@r�\@r-@qG�@q�@p�9@o�@o��@n�y@nv�@m�-@mO�@m�@l��@k��@kdZ@k"�@k"�@k"�@ko@j�@j~�@j�@iX@g�;@f�@fff@fE�@f@e��@e�h@e/@dz�@d�@c��@cdZ@b��@b^5@a�#@ahs@a&�@a%@`�`@`Ĝ@`1'@_
=@^E�@^{@]�-@\�/@\��@\��@\��@\��@\��@\�D@\1@[��@[t�@[33@Z�@Z~�@Zn�@Z^5@ZJ@Yx�@Y�@X��@X  @W�;@W�P@Vȴ@U�h@U?}@U/@U/@U�@T�/@T��@TI�@S��@S�
@S�F@SS�@R�@R�@R��@RM�@Qhs@Q&�@Q�@Q%@P�9@PQ�@P1'@P  @O;d@N��@Nȴ@N��@Mp�@L�@LI�@K�F@K��@Kt�@KC�@K@J��@J-@I��@I��@I�7@IX@H�`@Hb@F�@F�R@F�R@F�R@F�+@E�@EV@D�/@D�j@D�@D�D@D�D@D(�@Cƨ@C"�@Bn�@BM�@BJ@A��@A��@Ahs@AX@AG�@A�@@�`@@�@@bN@@A�@@1'@@b@?+@>��@>E�@>@=��@=�-@=�@=�@=V@<�@<�D@<1@;dZ@:�@:��@:�\@:�@9�^@9��@9�7@9hs@9�@81'@7��@7��@7|�@7\)@7
=@6�R@65?@5�@5�-@5/@4��@4Z@3�
@3��@3t�@3S�@3@2�H@2�H@2��@2�!@2�\@2�\@2~�@2=q@2-@2J@1�@1�^@1G�@1&�@0��@0�u@0r�@0 �@/�@/;d@/
=@.�y@.ȴ@.��@.��@.v�@.v�@.v�@.V@.5?@.$�@-�T@,��@,1@+ƨ@+�@+S�@+33@+@*�H@*��@*�\@*^5@*�@)�#@)��@)G�@)7L@)7L@)7L@)�@(��@(��@(Ĝ@(��@(r�@(1'@'��@'l�@'+@'
=@&��@&�@&�R@&��@&��@&V@&{@&@%�-@%�@%�@%V@$�@$��@$��@$��@$(�@#ƨ@#ƨ@#��@#dZ@#o@"��@"��@"n�@"=q@"�@"J@!��@!�7@!�@ �`@ ��@ r�@ bN@ A�@  �@�@�;@�@��@��@�P@|�@l�@K�@+@�y@�R@v�@{@�@/@�/@I�@�F@��@��@��@��@�@C�@33@"�@�H@��@^5@=q@-@�@J@�@��@��@�^@��@��@hs@��@��@��@��@�u@b@�w@��@\)@��@�@��@v�@V@$�@�@�T@�T@�T@�T@��@��@@�-@�h@p�@?}@V@�/@��@Z@1@�
@��@33@�@n�@=q@-@J@�@��@�7@x�@7L@�@�`@Q�@b111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
aHB
`BB
`BB
`BB
`BB
bNB
hsB
jB
n�B
o�B
q�B
t�B
x�B
�B
�bB
�bB
�VB
�%B
~�B
�+B
�bB
��B
ŢB
�ZB
��B	7B�B33BI�BJ�BK�BP�B\)Be`Bv�B�-B�B�B\BǮB�=Bv�Bx�B��BǮBǮB��B�RBBƨBƨB��B�HB�B�mB�B�yB�fB�B�B�NB��BB�dB�B��B��B�Bn�BaHBQ�BH�B1'B �BB
�)B
�B
�%B
_;B
E�B
)�B
{B	��B	ÖB	�3B	��B	��B	��B	��B	�VB	m�B	dZB	aHB	T�B	J�B	D�B	>wB	/B	�B		7B��B��B�B�B�B�TB�NB�5B�)B�B�B��B��B��B�
B�
B��B��B��B��B�B�B�B�B�BB�B	+B	VB	JB	
=B	�B	�B	�B	{B	\B	+B��B��B��B��B	B	+B	VB	�B	�B	%�B	'�B	)�B	+B	&�B	#�B	1'B	=qB	C�B	B�B	@�B	?}B	;dB	;dB	>wB	A�B	?}B	B�B	E�B	C�B	E�B	H�B	I�B	H�B	K�B	L�B	P�B	T�B	XB	^5B	`BB	bNB	aHB	e`B	l�B	p�B	q�B	t�B	u�B	x�B	{�B	|�B	|�B	|�B	~�B	�B	�%B	�+B	�%B	�B	�B	�B	�B	�B	�1B	�VB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�'B	�3B	�-B	�3B	�3B	�9B	�9B	�9B	�-B	�-B	�3B	�3B	�3B	�3B	�FB	�FB	�LB	�LB	�RB	�XB	�^B	�dB	�dB	�dB	�dB	�dB	�qB	��B	B	B	B	ÖB	ÖB	ÖB	ĜB	ŢB	ǮB	ǮB	ȴB	ȴB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�#B	�/B	�/B	�/B	�/B	�)B	�/B	�/B	�/B	�5B	�5B	�;B	�BB	�BB	�BB	�BB	�HB	�HB	�HB	�HB	�HB	�HB	�HB	�NB	�NB	�HB	�NB	�HB	�HB	�NB	�ZB	�TB	�fB	�yB	�B	�B	�B	�B	�B	�B	�B	�yB	�sB	�mB	�mB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
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
1B
1B
	7B

=B

=B

=B
DB
PB
PB
PB
PB
VB
VB
VB
VB
VB
VB
\B
\B
\B
bB
bB
bB
hB
hB
hB
hB
hB
oB
oB
oB
oB
oB
oB
uB
{B
{B
{B
{B
{B
{B
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
 �B
 �B
 �B
�B
�B
 �B
 �B
!�B
!�B
!�B
!�B
!�B
!�B
"�B
#�B
#�B
#�B
$�B
$�B
%�B
%�B
&�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
'�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
+B
+B
+B
+B
+B
,B
,B
,B
,B
,B
-B
-B
.B
.B
.B
/B
/B
/B
0!B
0!B
0!B
0!B
0!B
1'B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
33B
33B
33B
33B
49B
5?B
6FB
6FB
6FB
6FB
6FB
6FB
6FB
7LB
8RB
8RB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
:^B
;dB
;dB
;dB
;dB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
=qB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
?}B
?}B
?}B
?}B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
B�B
A�B
A�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
H�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
S�B
T�B
T�B
VB
VB
VB
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
YB
YB
YB
ZB
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
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
\)B
\)B
[#B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
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
dZB
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
ffB
ffB
ffB
ffB
ffB
gmB
gmB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
jB
jB
jB
jB
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
u�B
t�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
aHB
`BB
`BB
`BB
`BB
bNB
hsB
jB
n�B
o�B
q�B
t�B
x�B
�B
�bB
�bB
�VB
�%B
~�B
�+B
�bB
��B
ŢB
�ZB
��B	7B�B33BI�BJ�BK�BP�B\)Be`Bv�B�-B�B�B\BǮB�=Bv�Bx�B��BǮBǮB��B�RBBƨBƨB��B�HB�B�mB�B�yB�fB�B�B�NB��BB�dB�B��B��B�Bn�BaHBQ�BH�B1'B �BB
�)B
�B
�%B
_;B
E�B
)�B
{B	��B	ÖB	�3B	��B	��B	��B	��B	�VB	m�B	dZB	aHB	T�B	J�B	D�B	>wB	/B	�B		7B��B��B�B�B�B�TB�NB�5B�)B�B�B��B��B��B�
B�
B��B��B��B��B�B�B�B�B�BB�B	+B	VB	JB	
=B	�B	�B	�B	{B	\B	+B��B��B��B��B	B	+B	VB	�B	�B	%�B	'�B	)�B	+B	&�B	#�B	1'B	=qB	C�B	B�B	@�B	?}B	;dB	;dB	>wB	A�B	?}B	B�B	E�B	C�B	E�B	H�B	I�B	H�B	K�B	L�B	P�B	T�B	XB	^5B	`BB	bNB	aHB	e`B	l�B	p�B	q�B	t�B	u�B	x�B	{�B	|�B	|�B	|�B	~�B	�B	�%B	�+B	�%B	�B	�B	�B	�B	�B	�1B	�VB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�'B	�3B	�-B	�3B	�3B	�9B	�9B	�9B	�-B	�-B	�3B	�3B	�3B	�3B	�FB	�FB	�LB	�LB	�RB	�XB	�^B	�dB	�dB	�dB	�dB	�dB	�qB	��B	B	B	B	ÖB	ÖB	ÖB	ĜB	ŢB	ǮB	ǮB	ȴB	ȴB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�#B	�/B	�/B	�/B	�/B	�)B	�/B	�/B	�/B	�5B	�5B	�;B	�BB	�BB	�BB	�BB	�HB	�HB	�HB	�HB	�HB	�HB	�HB	�NB	�NB	�HB	�NB	�HB	�HB	�NB	�ZB	�TB	�fB	�yB	�B	�B	�B	�B	�B	�B	�B	�yB	�sB	�mB	�mB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
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
1B
1B
	7B

=B

=B

=B
DB
PB
PB
PB
PB
VB
VB
VB
VB
VB
VB
\B
\B
\B
bB
bB
bB
hB
hB
hB
hB
hB
oB
oB
oB
oB
oB
oB
uB
{B
{B
{B
{B
{B
{B
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
 �B
 �B
 �B
�B
�B
 �B
 �B
!�B
!�B
!�B
!�B
!�B
!�B
"�B
#�B
#�B
#�B
$�B
$�B
%�B
%�B
&�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
'�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
+B
+B
+B
+B
+B
,B
,B
,B
,B
,B
-B
-B
.B
.B
.B
/B
/B
/B
0!B
0!B
0!B
0!B
0!B
1'B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
33B
33B
33B
33B
49B
5?B
6FB
6FB
6FB
6FB
6FB
6FB
6FB
7LB
8RB
8RB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
:^B
;dB
;dB
;dB
;dB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
=qB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
?}B
?}B
?}B
?}B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
B�B
A�B
A�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
H�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
S�B
T�B
T�B
VB
VB
VB
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
YB
YB
YB
ZB
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
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
\)B
\)B
[#B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
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
dZB
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
ffB
ffB
ffB
ffB
ffB
gmB
gmB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
jB
jB
jB
jB
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
u�B
t�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                         ' ( & # !                                             ! % & & $ !                   ! "                                                      " !                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            2 C�000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000   PRES            TEMP            PSAL            NB_SAMPLE_CTD   PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.13 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted during real time processing based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       20230609140105                                          AO  ARCAADJP                                                                    20230609140105    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20230609140105  QCP$                G�O�G�O�G�O�1F83E           AO  ARGQQCPL                                                                    20230609140105  QCF$                G�O�G�O�G�O�0               