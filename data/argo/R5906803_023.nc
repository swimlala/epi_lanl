CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-05-16T17:22:56Z creation      
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
resolution        =���   axis      Z        �  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  K�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  O�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  _   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  c   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  r�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �<   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �$   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �D   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �d   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �L   NB_SAMPLE_CTD            
         	long_name         2Number of samples in each pressure bin for the CTD     
_FillValue        �     units         count      C_format      %5d    FORTRAN_format        I5     
resolution                �  ��   NB_SAMPLE_CTD_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  @  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  8  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �$   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �(   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �h   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �x   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �|   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20230516172256  20230516172256  5906803 Argo PMEL                                                       GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            NB_SAMPLE_CTD      A   AO  9276                            2B  A   NAVIS_A                         1437                            170425                          863 @��vL1   @�>���@;>i�B���d_��n/1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         A   A   A       @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B?��BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(�fD)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6y�D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=�fD>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df�fDgfDg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D���D�<�D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�@�(�A{A"{AB{Ab{A�
=A�
=A�
=A�
=A�
=A�
=A�
=A�
=B �B�B�B�B �B(�B0�B8�B@�BH�BP�BX�B`�Bh�Bp�Bx�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�u�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C !HC!HC!HC!HC!HC
!HC!HC!HC!HC!HC!HC!HC!HC!HC!HC!HC !HC"!HC$!HC&!HC(!HC*!HC,!HC.!HC0!HC2!HC4!HC6!HC8!HC:!HC<!HC>!HC@!HCB!HCD!HCF!HCH!HCJ!HCL!HCN!HCP!HCR!HCT!HCV!HCX!HCZ!HC\!HC^!HC`!HCb!HCd!HCf!HCh!HCj!HCl!HCn!HCp!HCr!HCt!HCv!HCx!HCz!HC|!HC~!HC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�qC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�qC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D RD �RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RD	RD	�RD
RD
�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RD RD �RD!RD!�RD"RD"�RD#RD#�RD$RD$�RD%RD%�RD&RD&�RD'RD'�RD(RD(��D)RD)�RD*RD*�RD+RD+�RD,RD,�RD-RD-�RD.RD.�RD/RD/�RD0RD0�RD1RD1�RD2RD2�RD3RD3�RD4RD4�RD5RD5�RD6RD6��D7RD7�RD8RD8�RD9RD9�RD:RD:�RD;RD;�RD<RD<�RD=RD=��D>RD>�RD?RD?�RD@RD@�RDARDA�RDBRDB�RDCRDC�RDDRDD�RDERDE�RDFRDF�RDGRDG�RDHRDH�RDIRDI�RDJRDJ�RDKRDK�RDLRDL�RDMRDM�RDNRDN�RDORDO�RDPRDP�RDQRDQ�RDRRDR�RDSRDS�RDTRDT�RDURDU�RDVRDV�RDWRDW�RDXRDX�RDYRDY�RDZRDZ�RD[RD[�RD\RD\�RD]RD]�RD^RD^�RD_RD_�RD`RD`�RDaRDa�RDbRDb�RDcRDc�RDdRDd�RDeRDe�RDfRDf��Dg�Dg�RDhRDh�RDiRDi�RDjRDj�RDkRDk�RDlRDl�RDmRDm�RDnRDn�RDoRDo�RDpRDp�RDqRDq�RDrRDr�RDsRDs�RDtRDt�RDuRDu�RDvRDv�RDwRDw�RDxRDx�RDyRDy�RDzRDz�RD{RD{�RD|RD|�RD}RD}�RD~RD~�RDRD�RD�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D� �D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��\D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D)D��)D�)D�D)DÄ)D��)D�)D�D)DĄ)D��)D�)D�D)Dń)D��)D�)D�D)DƄ)D��)D�)D�D)DǄ)D��)D�)D�D)DȄ)D��)D�)D�D)DɄ)D��)D�)D�D)Dʄ)D��)D�)D�D)D˄)D��)D�)D�D)D̄)D��)D�)D�D)D̈́)D��)D�)D�D)D΄)D��)D�)D�D)Dτ)D��)D�)D�D)DЄ)D��)D�)D�D)Dф)D��)D�)D�D)D҄)D��)D�)D�D)Dӄ)D��)D�)D�D)DԄ)D��)D�)D�D)DՄ)D��)D�)D�D)Dք)D��)D�)D�D)Dׄ)D��)D�)D�D)D؄)D��)D�)D�D)Dل)D��)D�)D�D)Dڄ)D��)D�)D�D)Dۄ)D��)D� �D�@�D܄)D��)D�)D�D)D݄)D��)D�)D�D)Dބ)D��)D�)D�D)D߄)D��)D�)D�D)D��)D��)D�)D�D)D�)D��)D�)D�D)D�)D��)D�)D�D)D�)D��)D�)D�D)D�)D��)D�)D�D)D�)D��)D�)D�D)D��D���D�)D�D)D�)D��)D�)D�D)D�)D��)D�)D�D)D�)D��)D�)D�D)D�)D��)D�)D�D)D�)D��)D�)D�D)D�)D��)D�)D�D)D�)D��)D�)D�D)D�\D��)D�)D�D)D�)D��)D�)D�D)D��)D��)D�)D�D)D�)D��)D�)D�D)D�)D��)D�)D�D)D�)D��)D�)D�D)D�)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��)D�)D�D)D��)D��\D� �111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A�  A�A�A�A�  A���A�A���A���A��A��A���A���A���A��A��A���A���A���A��A��;A��#A��
A��A���A��A��HA���A���A�ȴA�ȴA��
A�ȴA��!A���A���A�t�A�-A�
=A��-A��A��7A�I�A�$�A�A�=qA���A��A��wA�{A��A��A�-A�\)A��A�x�A��A���A���A��A��;A�33A�hsA�$�A���A��A�=qA�%A���A��!A���A���A�A�A���A���A��/A�bA�K�A�(�A�ƨA�S�A�z�A� �A��#A���A�~�A��!A��yA��A�r�A��+A�9XA��yA���A�\)A��A���A�1A��!A��/A�n�A��A~ZA{��Az��AwoAt�Ar�Aq�TAq�-Aq&�Ap�DAot�AnbNAm\)Al-AjM�Ai+Ah��Ah1'Ag`BAf�DAep�Ad��AcƨAc?}Ab~�A_�7A[\)AXȴAX�AWdZAU�^AS�AQ�wAP��APv�APbAOXAOANAM��AL��AKdZAJ��AJ �AI�AH��AG�mAG�PAG+AF�AEO�AC��AB��AB �AA��AA/A@�/A@VA?p�A=�
A:�A8��A89XA7x�A6��A6bNA6=qA5�-A4�yA3?}A2��A1�hA0�9A0(�A/G�A-��A-x�A-�A,��A,Q�A+�A+7LA*�+A)x�A'dZA&�\A&^5A%XA$�A$��A$I�A"��A!dZA!+A!oA �`AƨAA�
A(�AS�A
=A�A�yA�9AE�A��A"�A�wA�;A�A��A��A��A�mAVA�uAjA$�A�
A7LA��A(�A|�An�AG�A
�\A
$�A	��A	l�A��A��A��AhsA��A  A�FA�A�\AAG�A��A�TAt�A ��@��F@��7@�bN@��
@��@��@�R@��@�|�@�7L@�S�@�5?@�9@�
=@�G�@�1'@�M�@�z�@�C�@ڇ+@�$�@�@�?}@ش9@ו�@���@�%@���@ԓu@�1@ӍP@�o@�ff@�@�ƨ@��@�=q@��@�p�@ˮ@�hs@�Q�@�;d@�~�@�C�@��@�
=@�ȴ@��!@�~�@�5?@�@�Ĝ@�=q@��j@��w@�o@�ȴ@���@�~�@�^5@�5?@�{@���@�z�@�^5@���@��@���@��@�+@��@�ȴ@�$�@�z�@��@���@��
@�@�{@�hs@���@�\)@�\)@�K�@�S�@�S�@�S�@�K�@�K�@�;d@�~�@�$�@�{@��T@�7L@��@�A�@���@�l�@�K�@�;d@��@���@��\@��@��u@���@�@���@�^5@�5?@���@���@��D@�(�@��w@��\@�$�@�-@��@�`B@�%@��`@��@�;d@��@��@�E�@�{@�hs@��9@��D@�I�@��@�t�@�~�@�J@���@��-@��@�@��h@��@���@�9X@��@�ȴ@���@��\@�V@���@���@�`B@�V@� �@��P@�dZ@�n�@�5?@�{@�@���@��@�hs@��@��@�Z@�A�@�b@�  @�9X@��@�Ĝ@�bN@��D@��@���@���@��m@��F@���@��@���@���@���@�t�@�33@��@���@�=q@�$�@�J@���@���@���@�%@��@�Z@�A�@� �@�1@��;@�33@���@��R@��H@�ȴ@��R@�v�@�{@��@�hs@��@���@�z�@�A�@�;@l�@�@~�y@~�R@~v�@~E�@}��@}@}��@}�@|�/@|I�@{ƨ@{o@z~�@zn�@zJ@y�@y��@y&�@y%@x�`@x�u@xr�@x1'@x  @w��@wK�@v�R@vV@v{@u@up�@uO�@t��@tj@st�@r��@r~�@r=q@q��@q�^@qX@q%@pĜ@p��@p�@p�@pr�@pbN@pr�@pbN@pr�@pr�@pr�@pbN@pbN@pQ�@pA�@p �@o�P@n��@l��@l�D@l�D@lz�@l(�@kt�@k"�@j��@j�!@j��@j^5@jM�@j-@i��@h�u@hb@g��@g|�@g;d@fȴ@fV@e��@eV@d�/@d�/@d�/@d��@dI�@ct�@cdZ@cC�@c@b��@b=q@a�^@`��@`�9@`�u@`Q�@_�;@_;d@^�@^�+@]��@\�D@[��@Z�H@Z=q@Y�@Y��@Y��@Y��@Yhs@Y7L@X��@X�@W�P@V��@Vȴ@V��@V�R@V�@V��@V�y@Vȴ@Vv�@VV@VV@VE�@V5?@U@U�@T�@Tz�@TZ@TI�@TI�@S��@SC�@So@R��@Rn�@Rn�@R^5@R-@Q��@QG�@P��@O�@O\)@N�R@M@M�@MO�@M/@L�/@K�m@KdZ@K"�@Ko@J�@J-@I�@I�^@I��@I�7@Ihs@IG�@H��@HA�@Gl�@G�@F�y@F��@Fff@FE�@F$�@F{@E�@E��@EV@D9X@C�m@Ct�@CC�@B�@B�\@B^5@B�@@�`@@1'@?�@?�@?�@?�@?�;@?��@?��@?|�@?l�@?�@>ȴ@>V@>V@>E�@>5?@=�T@=?}@=V@<�@<�j@<��@<��@<j@<I�@;ƨ@;33@:��@:��@:��@:n�@9�#@9%@8Ĝ@8��@8r�@8A�@7�@7�@7l�@7;d@7�@7�@6�@6ȴ@6ȴ@6ȴ@6�R@6v�@65?@6{@5��@5`B@5O�@5?}@5V@4��@4�/@4�/@4��@4�j@4��@4�D@4z�@4Z@49X@4(�@4�@3��@3�m@3ƨ@3�F@3��@3t�@3C�@3o@2�@2�!@2�\@2�\@2^5@1hs@1�@0�9@0�u@0�@0b@/�w@/�P@/|�@/K�@/+@.�y@.��@.ff@.E�@.$�@.@-��@-�h@-/@,��@,Z@+�m@+��@+dZ@+33@+"�@*�@*�@*-@*�@)�@)��@)�7@)G�@)�@(��@(��@(�@(Q�@(1'@(1'@(1'@(b@(  @'��@'�w@'��@'l�@';d@'
=@&�@&�R@&��@&�+@&@%�h@%p�@%?}@%/@$��@$��@$�/@$�@$�@$��@$(�@#�
@#��@#dZ@#C�@#"�@"�@"=q@"J@!�@!�@!��@!x�@!X@!�@ �u@ A�@�@��@�@��@�P@\)@K�@��@��@v�@@@�h@?}@�@�/@z�@Z@I�@(�@1@�m@ƨ@�@S�@"�@�@M�@-@�#@��@�7@hs@&�@%@%@��@r�@Q�@1'@ �@b@|�@+@�@�@��@5?@{@@�@�-@p�@�/@��@�@��@t�@"�@�H@�!@��@�!@�!@�!@��@~�@n�@M�@��@��@��@��@hs@G�@&�@%@�`@��@��@�9@��@��@�u@r�@r�@bN@Q�@1'@ �@ �@b@  @�;@|�@;d@�y@�+@ff@E�@5?@@@�@p�@O�@?}@/@V@�@�@�D@I�@(�@ƨ@dZ@C�@@
��@
=q@
=q@
�@	��@	��@	�^@	��@	��@	��@	��@	�7@	�7@	x�@	x�@	hs@	hs@	hs@	X@	G�@	&�@�9@r�@A�@b@��@��@�P@�P@l�@l�@K�@;d@�y@ȴ@��@v�@@�-@p�@`B@`B@O�@/@V@�@�j@�D@9X@(�@(�@�@��@��@C�@"�@o@o@o111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A�  A�A�A�A�  A���A�A���A���A��A��A���A���A���A��A��A���A���A���A��A��;A��#A��
A��A���A��A��HA���A���A�ȴA�ȴA��
A�ȴA��!A���A���A�t�A�-A�
=A��-A��A��7A�I�A�$�A�A�=qA���A��A��wA�{A��A��A�-A�\)A��A�x�A��A���A���A��A��;A�33A�hsA�$�A���A��A�=qA�%A���A��!A���A���A�A�A���A���A��/A�bA�K�A�(�A�ƨA�S�A�z�A� �A��#A���A�~�A��!A��yA��A�r�A��+A�9XA��yA���A�\)A��A���A�1A��!A��/A�n�A��A~ZA{��Az��AwoAt�Ar�Aq�TAq�-Aq&�Ap�DAot�AnbNAm\)Al-AjM�Ai+Ah��Ah1'Ag`BAf�DAep�Ad��AcƨAc?}Ab~�A_�7A[\)AXȴAX�AWdZAU�^AS�AQ�wAP��APv�APbAOXAOANAM��AL��AKdZAJ��AJ �AI�AH��AG�mAG�PAG+AF�AEO�AC��AB��AB �AA��AA/A@�/A@VA?p�A=�
A:�A8��A89XA7x�A6��A6bNA6=qA5�-A4�yA3?}A2��A1�hA0�9A0(�A/G�A-��A-x�A-�A,��A,Q�A+�A+7LA*�+A)x�A'dZA&�\A&^5A%XA$�A$��A$I�A"��A!dZA!+A!oA �`AƨAA�
A(�AS�A
=A�A�yA�9AE�A��A"�A�wA�;A�A��A��A��A�mAVA�uAjA$�A�
A7LA��A(�A|�An�AG�A
�\A
$�A	��A	l�A��A��A��AhsA��A  A�FA�A�\AAG�A��A�TAt�A ��@��F@��7@�bN@��
@��@��@�R@��@�|�@�7L@�S�@�5?@�9@�
=@�G�@�1'@�M�@�z�@�C�@ڇ+@�$�@�@�?}@ش9@ו�@���@�%@���@ԓu@�1@ӍP@�o@�ff@�@�ƨ@��@�=q@��@�p�@ˮ@�hs@�Q�@�;d@�~�@�C�@��@�
=@�ȴ@��!@�~�@�5?@�@�Ĝ@�=q@��j@��w@�o@�ȴ@���@�~�@�^5@�5?@�{@���@�z�@�^5@���@��@���@��@�+@��@�ȴ@�$�@�z�@��@���@��
@�@�{@�hs@���@�\)@�\)@�K�@�S�@�S�@�S�@�K�@�K�@�;d@�~�@�$�@�{@��T@�7L@��@�A�@���@�l�@�K�@�;d@��@���@��\@��@��u@���@�@���@�^5@�5?@���@���@��D@�(�@��w@��\@�$�@�-@��@�`B@�%@��`@��@�;d@��@��@�E�@�{@�hs@��9@��D@�I�@��@�t�@�~�@�J@���@��-@��@�@��h@��@���@�9X@��@�ȴ@���@��\@�V@���@���@�`B@�V@� �@��P@�dZ@�n�@�5?@�{@�@���@��@�hs@��@��@�Z@�A�@�b@�  @�9X@��@�Ĝ@�bN@��D@��@���@���@��m@��F@���@��@���@���@���@�t�@�33@��@���@�=q@�$�@�J@���@���@���@�%@��@�Z@�A�@� �@�1@��;@�33@���@��R@��H@�ȴ@��R@�v�@�{@��@�hs@��@���@�z�@�A�@�;@l�@�@~�y@~�R@~v�@~E�@}��@}@}��@}�@|�/@|I�@{ƨ@{o@z~�@zn�@zJ@y�@y��@y&�@y%@x�`@x�u@xr�@x1'@x  @w��@wK�@v�R@vV@v{@u@up�@uO�@t��@tj@st�@r��@r~�@r=q@q��@q�^@qX@q%@pĜ@p��@p�@p�@pr�@pbN@pr�@pbN@pr�@pr�@pr�@pbN@pbN@pQ�@pA�@p �@o�P@n��@l��@l�D@l�D@lz�@l(�@kt�@k"�@j��@j�!@j��@j^5@jM�@j-@i��@h�u@hb@g��@g|�@g;d@fȴ@fV@e��@eV@d�/@d�/@d�/@d��@dI�@ct�@cdZ@cC�@c@b��@b=q@a�^@`��@`�9@`�u@`Q�@_�;@_;d@^�@^�+@]��@\�D@[��@Z�H@Z=q@Y�@Y��@Y��@Y��@Yhs@Y7L@X��@X�@W�P@V��@Vȴ@V��@V�R@V�@V��@V�y@Vȴ@Vv�@VV@VV@VE�@V5?@U@U�@T�@Tz�@TZ@TI�@TI�@S��@SC�@So@R��@Rn�@Rn�@R^5@R-@Q��@QG�@P��@O�@O\)@N�R@M@M�@MO�@M/@L�/@K�m@KdZ@K"�@Ko@J�@J-@I�@I�^@I��@I�7@Ihs@IG�@H��@HA�@Gl�@G�@F�y@F��@Fff@FE�@F$�@F{@E�@E��@EV@D9X@C�m@Ct�@CC�@B�@B�\@B^5@B�@@�`@@1'@?�@?�@?�@?�@?�;@?��@?��@?|�@?l�@?�@>ȴ@>V@>V@>E�@>5?@=�T@=?}@=V@<�@<�j@<��@<��@<j@<I�@;ƨ@;33@:��@:��@:��@:n�@9�#@9%@8Ĝ@8��@8r�@8A�@7�@7�@7l�@7;d@7�@7�@6�@6ȴ@6ȴ@6ȴ@6�R@6v�@65?@6{@5��@5`B@5O�@5?}@5V@4��@4�/@4�/@4��@4�j@4��@4�D@4z�@4Z@49X@4(�@4�@3��@3�m@3ƨ@3�F@3��@3t�@3C�@3o@2�@2�!@2�\@2�\@2^5@1hs@1�@0�9@0�u@0�@0b@/�w@/�P@/|�@/K�@/+@.�y@.��@.ff@.E�@.$�@.@-��@-�h@-/@,��@,Z@+�m@+��@+dZ@+33@+"�@*�@*�@*-@*�@)�@)��@)�7@)G�@)�@(��@(��@(�@(Q�@(1'@(1'@(1'@(b@(  @'��@'�w@'��@'l�@';d@'
=@&�@&�R@&��@&�+@&@%�h@%p�@%?}@%/@$��@$��@$�/@$�@$�@$��@$(�@#�
@#��@#dZ@#C�@#"�@"�@"=q@"J@!�@!�@!��@!x�@!X@!�@ �u@ A�@�@��@�@��@�P@\)@K�@��@��@v�@@@�h@?}@�@�/@z�@Z@I�@(�@1@�m@ƨ@�@S�@"�@�@M�@-@�#@��@�7@hs@&�@%@%@��@r�@Q�@1'@ �@b@|�@+@�@�@��@5?@{@@�@�-@p�@�/@��@�@��@t�@"�@�H@�!@��@�!@�!@�!@��@~�@n�@M�@��@��@��@��@hs@G�@&�@%@�`@��@��@�9@��@��@�u@r�@r�@bN@Q�@1'@ �@ �@b@  @�;@|�@;d@�y@�+@ff@E�@5?@@@�@p�@O�@?}@/@V@�@�@�D@I�@(�@ƨ@dZ@C�@@
��@
=q@
=q@
�@	��@	��@	�^@	��@	��@	��@	��@	�7@	�7@	x�@	x�@	hs@	hs@	hs@	X@	G�@	&�@�9@r�@A�@b@��@��@�P@�P@l�@l�@K�@;d@�y@ȴ@��@v�@@�-@p�@`B@`B@O�@/@V@�@�j@�D@9X@(�@(�@�@��@��@C�@"�@o@o@o111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BXBXBXBXBXBXBXBXBXBW
BXBXBXBXBXBXBXBXBW
BXBXBW
BW
BVBW
BVBVBW
BVBVBVBVBVBVBS�BS�BR�BP�BN�BJ�BG�BE�BB�BB�BD�BB�BA�B?}B<jB+B��B�mB�B�^B{�BQ�B�B�B�B�B�fB�#B�B��B��BȴBÖB�}B�jB�LB�B��B�{B�\B�=B�Bz�Bp�Be`BW
BO�BI�B>wB8RB49B1'B.B$�B�BJBB
��B
�ZB
�5B
�#B
��B
B
�3B
��B
��B
��B
{�B
[#B
E�B
1'B
+B
�B
1B	��B	��B	��B	�B	�B	�yB	�TB	�)B	�B	��B	ĜB	��B	�qB	�XB	�9B	�B	�B	��B	��B	��B	�7B	p�B	aHB	\)B	XB	N�B	A�B	:^B	6FB	33B	1'B	.B	+B	&�B	#�B	�B	�B	�B	uB	hB	
=B	+B	B	B��B��B�B�B�yB�fB�ZB�HB�;B�B��BǮB�}B�dB�LB�FB�3B�3B�!B�B��B��B��B��B��B��B�{B�oB�hB�\B�VB�JB�=B�+B�B�B|�B{�By�Bw�Bu�Bt�Br�Bn�Bl�Bl�BjBiyBe`BcTB`BB]/B\)B[#B[#BZBYBW
BS�BQ�BL�BH�BG�BD�BB�BA�B?}B>wB=qB<jB;dB:^B9XB7LB7LB49B49B1'B0!B/B/B-B-B,B)�B)�B'�B&�B&�B%�B$�B#�B"�B!�B�B�B�B�B�B�B�B�B�B{BuBuBoBhBhBhBbB\BbBbBbBbBbBbBbB\BbBhBhBhBhBhBhBhBhBbBuBoBoBoBoBuB�B�B�B�B�B�B �B!�B!�B!�B!�B!�B#�B(�B+B.B/B/B0!B0!B0!B0!B0!B1'B33B8RB<jB>wB?}B>wB@�B@�B@�BB�BG�BH�BI�BH�BK�BM�BP�BS�BYBYBYBYBYBYBYBYBYB\)B]/B]/B^5BaHBdZBffBiyBk�Bl�Bl�Bl�Bl�Bm�Bv�Bw�B{�B|�B~�B~�B� B�B�1B�=B�PB�\B��B��B��B��B��B��B��B��B��B��B��B�'B�9B�9B�3B�9B�FB�RB�jB�qB�}B��BǮB��B��B��B�
B�B�B�)B�;B�HB�TB�ZB�fB�sB�sB�mB�B�B�B��B��B��B��B��B��B��B��B	B	B	B	B	%B	
=B	hB	{B	�B	�B	�B	!�B	&�B	)�B	/B	1'B	5?B	8RB	:^B	>wB	?}B	A�B	C�B	E�B	G�B	L�B	M�B	N�B	N�B	O�B	T�B	T�B	T�B	T�B	T�B	VB	W
B	YB	YB	[#B	`BB	bNB	bNB	e`B	gmB	jB	n�B	o�B	o�B	o�B	o�B	p�B	q�B	r�B	s�B	s�B	t�B	v�B	w�B	y�B	z�B	{�B	|�B	}�B	� B	�B	�B	�B	�B	�%B	�%B	�1B	�1B	�7B	�DB	�PB	�PB	�\B	�hB	�oB	�oB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�!B	�'B	�'B	�'B	�'B	�'B	�'B	�'B	�3B	�9B	�FB	�FB	�LB	�LB	�LB	�RB	�RB	�^B	�^B	�^B	�jB	�jB	�jB	�wB	B	ĜB	ŢB	ƨB	ƨB	ǮB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�#B	�)B	�/B	�/B	�5B	�BB	�TB	�`B	�`B	�fB	�mB	�mB	�mB	�mB	�mB	�mB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
%B
%B
+B
+B
1B
	7B
	7B
	7B

=B

=B

=B
DB
JB
VB
VB
\B
\B
bB
bB
bB
bB
hB
hB
oB
{B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
"�B
"�B
"�B
"�B
"�B
#�B
$�B
%�B
%�B
&�B
&�B
'�B
(�B
)�B
)�B
)�B
)�B
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
0!B
0!B
0!B
1'B
1'B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
33B
33B
33B
49B
33B
49B
5?B
6FB
6FB
7LB
6FB
7LB
8RB
8RB
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
<jB
<jB
=qB
=qB
>wB
>wB
>wB
>wB
>wB
?}B
?}B
@�B
@�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
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
E�B
E�B
E�B
E�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
M�B
M�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
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
XB
XB
XB
XB
XB
YB
YB
YB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
\)B
\)B
\)B
\)B
]/B
]/B
^5B
^5B
_;B
_;B
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
aHB
aHB
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
dZB
dZB
dZB
dZB
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
hsB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
jB
jB
jB
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BXBXBXBXBXBXBXBXBXBW
BXBXBXBXBXBXBXBXBW
BXBXBW
BW
BVBW
BVBVBW
BVBVBVBVBVBVBS�BS�BR�BP�BN�BJ�BG�BE�BB�BB�BD�BB�BA�B?}B<jB+B��B�mB�B�^B{�BQ�B�B�B�B�B�fB�#B�B��B��BȴBÖB�}B�jB�LB�B��B�{B�\B�=B�Bz�Bp�Be`BW
BO�BI�B>wB8RB49B1'B.B$�B�BJBB
��B
�ZB
�5B
�#B
��B
B
�3B
��B
��B
��B
{�B
[#B
E�B
1'B
+B
�B
1B	��B	��B	��B	�B	�B	�yB	�TB	�)B	�B	��B	ĜB	��B	�qB	�XB	�9B	�B	�B	��B	��B	��B	�7B	p�B	aHB	\)B	XB	N�B	A�B	:^B	6FB	33B	1'B	.B	+B	&�B	#�B	�B	�B	�B	uB	hB	
=B	+B	B	B��B��B�B�B�yB�fB�ZB�HB�;B�B��BǮB�}B�dB�LB�FB�3B�3B�!B�B��B��B��B��B��B��B�{B�oB�hB�\B�VB�JB�=B�+B�B�B|�B{�By�Bw�Bu�Bt�Br�Bn�Bl�Bl�BjBiyBe`BcTB`BB]/B\)B[#B[#BZBYBW
BS�BQ�BL�BH�BG�BD�BB�BA�B?}B>wB=qB<jB;dB:^B9XB7LB7LB49B49B1'B0!B/B/B-B-B,B)�B)�B'�B&�B&�B%�B$�B#�B"�B!�B�B�B�B�B�B�B�B�B�B{BuBuBoBhBhBhBbB\BbBbBbBbBbBbBbB\BbBhBhBhBhBhBhBhBhBbBuBoBoBoBoBuB�B�B�B�B�B�B �B!�B!�B!�B!�B!�B#�B(�B+B.B/B/B0!B0!B0!B0!B0!B1'B33B8RB<jB>wB?}B>wB@�B@�B@�BB�BG�BH�BI�BH�BK�BM�BP�BS�BYBYBYBYBYBYBYBYBYB\)B]/B]/B^5BaHBdZBffBiyBk�Bl�Bl�Bl�Bl�Bm�Bv�Bw�B{�B|�B~�B~�B� B�B�1B�=B�PB�\B��B��B��B��B��B��B��B��B��B��B��B�'B�9B�9B�3B�9B�FB�RB�jB�qB�}B��BǮB��B��B��B�
B�B�B�)B�;B�HB�TB�ZB�fB�sB�sB�mB�B�B�B��B��B��B��B��B��B��B��B	B	B	B	B	%B	
=B	hB	{B	�B	�B	�B	!�B	&�B	)�B	/B	1'B	5?B	8RB	:^B	>wB	?}B	A�B	C�B	E�B	G�B	L�B	M�B	N�B	N�B	O�B	T�B	T�B	T�B	T�B	T�B	VB	W
B	YB	YB	[#B	`BB	bNB	bNB	e`B	gmB	jB	n�B	o�B	o�B	o�B	o�B	p�B	q�B	r�B	s�B	s�B	t�B	v�B	w�B	y�B	z�B	{�B	|�B	}�B	� B	�B	�B	�B	�B	�%B	�%B	�1B	�1B	�7B	�DB	�PB	�PB	�\B	�hB	�oB	�oB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�!B	�'B	�'B	�'B	�'B	�'B	�'B	�'B	�3B	�9B	�FB	�FB	�LB	�LB	�LB	�RB	�RB	�^B	�^B	�^B	�jB	�jB	�jB	�wB	B	ĜB	ŢB	ƨB	ƨB	ǮB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�#B	�)B	�/B	�/B	�5B	�BB	�TB	�`B	�`B	�fB	�mB	�mB	�mB	�mB	�mB	�mB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
%B
%B
+B
+B
1B
	7B
	7B
	7B

=B

=B

=B
DB
JB
VB
VB
\B
\B
bB
bB
bB
bB
hB
hB
oB
{B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
"�B
"�B
"�B
"�B
"�B
#�B
$�B
%�B
%�B
&�B
&�B
'�B
(�B
)�B
)�B
)�B
)�B
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
0!B
0!B
0!B
1'B
1'B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
33B
33B
33B
49B
33B
49B
5?B
6FB
6FB
7LB
6FB
7LB
8RB
8RB
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
<jB
<jB
=qB
=qB
>wB
>wB
>wB
>wB
>wB
?}B
?}B
@�B
@�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
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
E�B
E�B
E�B
E�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
M�B
M�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
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
XB
XB
XB
XB
XB
YB
YB
YB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
\)B
\)B
\)B
\)B
]/B
]/B
^5B
^5B
_;B
_;B
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
aHB
aHB
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
dZB
dZB
dZB
dZB
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
hsB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
jB
jB
jB
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         $ e�000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000 PRES            TEMP            PSAL            NB_SAMPLE_CTD   PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.13 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted during real time processing based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       20230516172256                                          AO  ARCAADJP                                                                    20230516172256    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20230516172256  QCP$                G�O�G�O�G�O�1F83E           AO  ARGQQCPL                                                                    20230516172256  QCF$                G�O�G�O�G�O�0               