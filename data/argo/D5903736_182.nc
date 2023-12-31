CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T04:11:57Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         (Matthew Alkire, University of Washington      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7,   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  74   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7t   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     88   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8X   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8x   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8|   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
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
resolution        :�o     �  qt   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �\   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �\   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �P   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �$   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �(   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �,Argo profile    3.1 1.2 19500101000000  20181121041157  20190604094027  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051                            2C  D   APEX                            5368                            041511                          846 @��ς1   @����(@4��l�C��d۾vȴ91   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @���@�  @���A   A@  A`  A~ffA�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,�C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D
��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DR��DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� DifDi� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt` Dy]qD��)D�8 D��
D�ȤD�=D�L{D���D��
D�=D�S3D�k�D��3D� D�:�DڍD��{D��D�G
D�D��=11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��R@�A\)A;\)A[\)AyA��A��A��A��AͮAݮA��A��B�
B�
B�
B�
B&�
B.�
B6�
B>�
BF�
BN�
BV�
B^�
Bf�
Bn�
Bv�
B~�
B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B���B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+�]C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D mqD �qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qD	mqD	�qD
mqD
�DmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qD mqD �qD!mqD!�qD"mqD"�qD#mqD#�qD$mqD$�qD%mqD%�qD&mqD&�qD'mqD'�qD(mqD(�qD)mqD)�qD*mqD*�qD+mqD+�qD,mqD,�qD-mqD-�qD.mqD.�qD/mqD/�qD0mqD0�qD1mqD1�qD2mqD2�qD3mqD3�qD4mqD4�qD5mqD5�qD6mqD6�qD7mqD7�qD8mqD8�qD9mqD9�qD:mqD:�qD;mqD;�qD<mqD<�qD=mqD=�qD>mqD>�qD?mqD?�qD@mqD@�qDAmqDA�qDBmqDB�qDCmqDC�qDDmqDD�qDEmqDE�qDFmqDF�qDGmqDG�qDHmqDH�qDImqDI�qDJmqDJ�qDKmqDK�qDLmqDL�qDMmqDM�qDNmqDN�qDOmqDO�qDPmqDP�qDQmqDQ�qDRmqDR�DSmqDS�qDTmqDT�qDUmqDU�qDVmqDV�qDWmqDW�qDXmqDX�qDYmqDY�qDZmqDZ�qD[mqD[�qD\mqD\�qD]mqD]�qD^mqD^�qD_mqD_�qD`mqD`�qDamqDa�qDbmqDb�qDcmqDc�qDdmqDd�qDemqDe�qDfmqDf�qDgmqDg�qDhmqDh��DimqDi�qDjmqDj�qDkmqDk�qDlmqDl�qDmmqDm�qDnmqDn�qDomqDo�qDpmqDp�qDqmqDq�qDrmqDr�qDsmqDs�qDtMqDyJ�D���D�.�D���D��\D��D�C3D�yGD���D��D�I�D�b�Dǹ�D��D�1�Dڃ�D��3D��D�=�D�z=D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�9XA�7LA�5?A�5?A�33A�&�A�9XA�E�A�C�A�E�A�E�A�E�A�I�A�G�A�S�A�S�A�S�A�S�A�VA�XA�XA�ZA�ZA�VA�O�A��A�
=A�M�A���A�v�A��`A�5?A��/A�x�A�&�A��`A�I�A��A���A�r�A���AʍPA�E�A���A�jA�M�A��AǴ9A�A�A�%AƼjAƅA�dZA�E�A�1'A�{A��A�VA�M�A���AÉ7A¼jA��;A�XA���A�ZA�5?A��#A�A��A��A��A�?}A���A��wA��jA�bNA�I�A�S�A���A�O�A�(�A���A��RA���A��
A�hsA��A��uA�z�A���A�hsA��#A�l�A���A�p�A��A�A�C�A��^A�%A�9XA�1'A���A�+A���A�l�A�A���A�&�A�ĜA�I�A��mA�O�A�ffA��hA�ZA��A�VA�7LA���A�1'A{Ay/Ax��Aw��Av�uAvA�Av �Au�At�DAr��Ar��ArffAq"�Ao��AnZAm�Al{Aj��Ai`BAhM�Af�HAe��Ae"�Ad��Ad1'Ac��Ac��Ab��Ab�Aa"�A]��AY�
AX�AW��AT��ARbAQoAP1ANĜAM�AJ��AI��AG�AE�#AE/AD�jAD�+AD(�AA�A@$�A?O�A>{A=\)A=/A=
=A<�A<ĜA<^5A;�hA:r�A9�#A8�A8$�A7S�A6�A5��A4��A4^5A3��A2��A2Q�A1oA0�uA0I�A/�
A.��A,ĜA,{A+�PA*�`A'�;A&-A%`BA#A"�RA!t�A I�A^5A��AA�AbA��A7LA1'A��A=qA�\A��AO�A�jA�^A��A$�A`BA��A�/A��A��AK�A?}A
v�A	�hA	&�A�A��A�A=qAC�A ��A bNA 9XA  �A �@���@�|�@�v�@��^@�  @�bN@��w@���@�@�7@�@�F@�+@��-@�&�@���@�b@�%@�h@⟾@��/@�E�@��@��@���@ٺ^@�r�@��H@Ձ@�9X@�|�@���@�=q@щ7@�z�@�|�@��@�n�@ͺ^@��@�ƨ@�l�@�C�@���@�`B@�j@� �@ǥ�@Ƈ+@�J@ũ�@�`B@�%@�(�@Å@���@�p�@�j@�1@�ƨ@���@�+@�5?@��-@�X@��@��@���@��9@�Q�@�b@�S�@�-@�V@���@��@�9X@��;@���@�=q@���@�O�@��@�%@�%@�%@���@���@���@���@�A�@�b@���@�dZ@�v�@��@�9X@��@���@�ȴ@��!@���@�~�@�ff@�{@���@��7@�G�@��`@�I�@��F@�\)@�\)@�S�@�K�@�ȴ@�ff@�=q@���@�x�@�X@�G�@�7L@�&�@��@�%@��j@�b@���@�dZ@�;d@��+@�@�@�@���@��j@�9X@�b@���@���@���@��@��@��R@���@���@�`B@�/@��j@��D@�1'@��@��w@�"�@��\@�v�@�^5@��@���@��-@��h@�7L@��@���@��@��`@��/@���@��D@�Q�@�A�@�A�@�1@��m@��w@��F@�l�@�ȴ@���@�V@�M�@�-@���@��h@��@���@��9@���@�j@��@�ƨ@���@���@�t�@�\)@�;d@�@��@�ȴ@���@��+@�n�@�^5@�=q@�{@���@���@���@��@�x�@�`B@�X@�7L@�%@��j@�bN@�  @�;d@��+@��#@�p�@�7L@�Ĝ@��u@��u@��u@���@��u@�r�@�bN@�Z@��;@�S�@��y@��y@��@���@�ff@�{@���@��T@���@���@���@�m]@|��@p�5@e�j@^��@X��@P��@C�@:�B@3y�@,  @$�K@!+�@�@��@m]@��@�H@	�)11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A�9XA�7LA�5?A�5?A�33A�&�A�9XA�E�A�C�A�E�A�E�A�E�A�I�A�G�A�S�A�S�A�S�A�S�A�VA�XA�XA�ZA�ZA�VA�O�A��A�
=A�M�A���A�v�A��`A�5?A��/A�x�A�&�A��`A�I�A��A���A�r�A���AʍPA�E�A���A�jA�M�A��AǴ9A�A�A�%AƼjAƅA�dZA�E�A�1'A�{A��A�VA�M�A���AÉ7A¼jA��;A�XA���A�ZA�5?A��#A�A��A��A��A�?}A���A��wA��jA�bNA�I�A�S�A���A�O�A�(�A���A��RA���A��
A�hsA��A��uA�z�A���A�hsA��#A�l�A���A�p�A��A�A�C�A��^A�%A�9XA�1'A���A�+A���A�l�A�A���A�&�A�ĜA�I�A��mA�O�A�ffA��hA�ZA��A�VA�7LA���A�1'A{Ay/Ax��Aw��Av�uAvA�Av �Au�At�DAr��Ar��ArffAq"�Ao��AnZAm�Al{Aj��Ai`BAhM�Af�HAe��Ae"�Ad��Ad1'Ac��Ac��Ab��Ab�Aa"�A]��AY�
AX�AW��AT��ARbAQoAP1ANĜAM�AJ��AI��AG�AE�#AE/AD�jAD�+AD(�AA�A@$�A?O�A>{A=\)A=/A=
=A<�A<ĜA<^5A;�hA:r�A9�#A8�A8$�A7S�A6�A5��A4��A4^5A3��A2��A2Q�A1oA0�uA0I�A/�
A.��A,ĜA,{A+�PA*�`A'�;A&-A%`BA#A"�RA!t�A I�A^5A��AA�AbA��A7LA1'A��A=qA�\A��AO�A�jA�^A��A$�A`BA��A�/A��A��AK�A?}A
v�A	�hA	&�A�A��A�A=qAC�A ��A bNA 9XA  �A �@���@�|�@�v�@��^@�  @�bN@��w@���@�@�7@�@�F@�+@��-@�&�@���@�b@�%@�h@⟾@��/@�E�@��@��@���@ٺ^@�r�@��H@Ձ@�9X@�|�@���@�=q@щ7@�z�@�|�@��@�n�@ͺ^@��@�ƨ@�l�@�C�@���@�`B@�j@� �@ǥ�@Ƈ+@�J@ũ�@�`B@�%@�(�@Å@���@�p�@�j@�1@�ƨ@���@�+@�5?@��-@�X@��@��@���@��9@�Q�@�b@�S�@�-@�V@���@��@�9X@��;@���@�=q@���@�O�@��@�%@�%@�%@���@���@���@���@�A�@�b@���@�dZ@�v�@��@�9X@��@���@�ȴ@��!@���@�~�@�ff@�{@���@��7@�G�@��`@�I�@��F@�\)@�\)@�S�@�K�@�ȴ@�ff@�=q@���@�x�@�X@�G�@�7L@�&�@��@�%@��j@�b@���@�dZ@�;d@��+@�@�@�@���@��j@�9X@�b@���@���@���@��@��@��R@���@���@�`B@�/@��j@��D@�1'@��@��w@�"�@��\@�v�@�^5@��@���@��-@��h@�7L@��@���@��@��`@��/@���@��D@�Q�@�A�@�A�@�1@��m@��w@��F@�l�@�ȴ@���@�V@�M�@�-@���@��h@��@���@��9@���@�j@��@�ƨ@���@���@�t�@�\)@�;d@�@��@�ȴ@���@��+@�n�@�^5@�=q@�{@���@���@���@��@�x�@�`B@�X@�7L@�%@��j@�bN@�  @�;d@��+@��#@�p�@�7L@�Ĝ@��u@��u@��u@���@��u@�r�@�bN@�Z@��;@�S�@��y@��y@��@���@�ff@�{@���@��TG�O�@���@���@�m]@|��@p�5@e�j@^��@X��@P��@C�@:�B@3y�@,  @$�K@!+�@�@��@m]@��@�H@	�)11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
%B
bB
33B
��B
�#BB �BA�BQ�BbNBv�B� B�B��B�B�FBĜB��B�B�;B�B�BBJBhB�B(�B2-B:^B<jB<jB<jB=qB?}BD�BK�BN�BR�BT�BiyBn�Bm�Bm�Bu�B�=B�bB��B��B�B�B��B��B��B��B�!B��B��B�BdZB_;BaHB_;BgmBaHBT�BN�BD�B6FB/B(�B$�B!�B �B�B�BPB%B�B�fBƨB�qB�9B��B�+B�Br�BiyBdZB]/BW
BL�B)�B�BDB
�sB
�B
ǮB
�B
�PB
p�B
ffB
cTB
]/B
VB
S�B
R�B
O�B
H�B
@�B
=qB
;dB
33B
)�B
"�B
�B
�B
VB
1B
B	��B	�B	�B	�B	�sB	�fB	�ZB	�BB	�#B	��B	�dB	��B	��B	�bB	{�B	l�B	e`B	^5B	T�B	J�B	>wB	7LB	-B	$�B	!�B	�B	�B	�B	\B		7B	B	B��B��B��B��B��B��B��B�B�B�B�sB�`B�NB�;B�)B�B�
B��B��B��B��B��BȴBĜB��B�wB�jB�RB�3B�B�B��B��B��B��B��B��B��B��B��B��B�hB�VB�JB�=B�1B�+B�B�B�B� B}�Bz�By�Bx�Bw�Bt�Br�Bp�Bp�Bp�Bo�Bm�BiyBffBgmBffBffBffBffBffBe`Be`BdZBcTBbNBdZBcTBcTBcTBdZBdZBdZBcTBdZBdZBcTBcTBffBiyBm�Bn�Br�Bw�B{�B|�B�B�B�B�B�B�B�B�1B��B��B��B��B��B��B��B�!B�9B�?B�?B�jB��B��BBǮBȴB��B��B��B��B��B��B�B�BB�NB�ZB�fB�mB�B�B�B�B�B��B��B��B��B��B��B	  B	  B	  B	B	B	%B		7B	
=B	VB	VB	\B	\B	\B	\B	bB	\B	bB	oB	oB	oB	{B	�B	�B	�B	 �B	'�B	+B	,B	,B	-B	-B	.B	/B	1'B	2-B	7LB	?}B	A�B	B�B	B�B	B�B	C�B	E�B	H�B	I�B	L�B	O�B	P�B	P�B	P�B	Q�B	Q�B	Q�B	S�B	YB	^5B	`BB	`BB	bNB	cTB	dZB	gmB	l�B	t�B	v�B	w�B	y�B	~�B	�B	�B	�B	�B	�B	�B	�7B	�=B	�JB	�PB	�VB	�bB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�'B	�3B	�3B	�3B	�9B	�FB	�LB	�LB	�XB	�wB	��B	B	B	ÖB	ŢB	ǮB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�)B	�/B	�/B	�5B	�5B	�5B	�;B	�;B	�;B	�HB	�TB	�ZB	�`B	�`B	�`B	�`B	�fB	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
�B
7B
$�B
-�B
7fB
=�B
BuB
G�B
QhB
YeB
Z�B
^�B
c�B
f�B
j�B
oiB
s3B
v�B
z�B
~�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�$B	�#B	�&B	�EB
�B
+OB
��B
�1B
�B�B9�BI�BZYBn�BxB}'B��B�B�OB��B��B�B�?B�B�B�BLB	gB�B �B*+B2[B4iB4hB4hB5rB7~B<�BC�BF�BJ�BL�BatBf�Be�Be�Bm�B�9B�_B��B��B�B�B��B��B��B��B�B��B��B}B\[BWCBYGBW?B_qBYOBL�BF�B<�B.KB' B �B�B�B�B�B�BYB�/B��B�wB��B��B�KB��B=ByBj�Ba�B\uBUDBO"BD�B"B�BeB
��B
�5B
��B
�3B
�yB
h�B
^�B
[�B
U[B
N0B
L"B
KB
H	B
@�B
8�B
5�B
3�B
+bB
"/B
B
�B
�B
�B
 gB	�AB	�B	��B	��B	��B	�B	ޜB	܎B	�xB	�]B	�,B	��B	�B	��B	��B	t(B	d�B	]�B	VuB	MDB	CB	6�B	/�B	%SB	"B	B	B	�B	�B	�B	�B�iB�LB�=B�<B�6B�0B�/B�B�
B��B��B��B�BݯBښBׇB�vB�eB�UB�CB�2B�!B�B�B�B��B��B��B��B��B��B�hB�_B�PB�CB�5B�'B�B�B��B��B��B��B��B��B��B��B��B�B}wB{gBzaBxVBvKBs:Br5Bq.Bp%BmBk
Bh�BiBh�Bg�Be�Ba�B^�B_�B^�B^�B^�B^�B^�B]�B]�B\�B[�BZ�B\�B[�B[�B[�B\�B\�B\�B[�B\�B\�B[�B[�B^�Ba�Be�Bf�BkBp-BtCBuKBybB|vB{rB{lBzjBzhB{pB��B��B��B�B�B�!B�)B�9B�~B��B��B��B��B��B��B��B�B�B�B�B�+B�9B�DB�XB�sB؞BڧBܳB޾B��B��B��B�B�B�B�B�&B�/B�3B�9B�JB�UB�XB�[B�]B�aB�|B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	
�B	
�B	
�B	�B	�B	�B	�B	B	 DB	#VB	$]B	$]B	%gB	%dB	&gB	'pB	)}B	*�B	/�B	7�B	9�B	:�B	:�B	:�B	;�B	=�B	AB	BB	E"B	H4B	I8B	I8B	I;B	J@B	J@B	J?B	LKB	QmB	V�B	X�B	X�B	Z�B	[�B	\�B	_�B	d�B	mB	oB	p!B	r1B	wOB	yXB	yZB	z]B	{gB	{bB	}pB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�#B	�)B	�4B	�JB	�MB	�XB	�XB	�^B	�_B	�cB	�wB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�$B	� B	�(B	�0B	�AB	�RB	�^B	�\B	�dB	�cB	�lB	�vB	�|B	�}B	փB	քB	ւB	ׇB	׈B	׈B	ٕB	۠B	ܥB	ݭB	ݮB	ݭB	ݫB	޴B	޴B	߾B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�%B	�!B	�&B	�*G�O�B	� B
>B
�B
EB
%�B
/�B
5�B
:�B
@-B
I�B
Q�B
S7B
V�B
\B
_5B
c1B
g�B
k~B
oB
r�B
wB11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.29 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0), vertically averaged dS =-0.008(+/-0.001) in PSS-78.                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Map scales: x=6,3; y=2,1. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                            201906040940272019060409402720190604094027  AO  ARCAADJP                                                                    20181121041157    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121041157  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121041157  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604094027  IP                  G�O�G�O�G�O�                