CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T04:15:19Z creation      
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
_FillValue                    �,Argo profile    3.1 1.2 19500101000000  20181121041519  20190604095259  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4055                            2C  D   APEX                            5374                            041511                          846 @׵8�1j1   @׵9�`Ҩ@;e`A�7L�c_��Q�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A���A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@ffBHffBP  BX  B_��Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:�C<�C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2fD2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtffDys3D��D�K�D��
D���D� D�M�D�x�D�� D� D�>�D�uD��{D��D�@ D�|)D���D�qD�F�D�k�D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�ff@�33A��A%��AE��Ae��A���A���A���A���AÙ�A���A���A���BffB	ffBffBffB!ffB)ffB1ffB9ffBA��BI��BQffBYffBa  BiffBqffByffB��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3Bĳ3Bȳ3B̳3Bг3BԳ3Bس3Bܳ3B�3B�3B�3B�3B�3B��3B��3B��3C Y�CY�CY�CY�CY�C
Y�CY�CY�CY�CY�CY�CY�CY�CY�CY�CY�C Y�C"Y�C$Y�C&Y�C(Y�C*Y�C,Y�C.Y�C0Y�C2Y�C4Y�C6Y�C8Y�C:s4C<s4C>Y�C@Y�CBY�CDY�CFY�CHY�CJY�CLY�CNY�CPY�CRY�CTY�CVY�CXY�CZY�C\Y�C^Y�C`Y�CbY�CdY�CfY�ChY�CjY�ClY�CnY�CpY�CrY�CtY�CvY�CxY�CzY�C|Y�C~Y�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�9�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�D fD �fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD	fD	�fD
fD
�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD fD �fD!fD!�fD"fD"�fD#fD#�fD$fD$�fD%fD%�fD&fD&�fD'fD'�fD(fD(�fD)fD)�fD*fD*�fD+fD+�fD,fD,�fD-fD-�fD.fD.�fD/fD/�fD0fD0�fD1fD1�fD2�D2�fD3fD3�fD4fD4�fD5fD5�fD6fD6�fD7fD7�fD8fD8�fD9fD9�fD:fD:�fD;fD;�fD<fD<�fD=fD=�fD>fD>�fD?fD?�fD@fD@�fDAfDA�fDBfDB�fDCfDC�fDDfDD�fDEfDE�fDFfDF�fDGfDG�fDHfDH�fDIfDI�fDJfDJ�fDKfDK�fDLfDL�fDMfDM�fDNfDN�fDOfDO�fDPfDP�fDQfDQ�fDRfDR�fDSfDS�fDTfDT�fDUfDU�fDVfDV�fDWfDW�fDXfDX�fDYfDY�fDZfDZ�fD[fD[�fD\fD\�fD]fD]�fD^fD^�fD_fD_�fD`fD`�fDafDa�fDbfDb�fDcfDc�fDdfDd�fDefDe�fDffDf�fDgfDg�fDhfDh�fDifDi�fDjfDj�fDkfDk�fDlfDl�fDmfDm�fDnfDn�fDofDo�fDpfDp�fDqfDq�fDrfDr�fDsfDs�fDtfDt|�Dy��D�"�D�V�D��=D��)D�3D�X�D���D��3D�#3D�I�D��RD�߮D�D�K3Dڇ\D��D��D�Q�D�w
D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�dZA�bNA�bNA�VA�-A�$�A�$�A�"�A� �A� �A��A��A��A��A��A��A��A��A�{A�bA�JA�%A���A���A��`A�ƨA�C�A�\)A���A�{A�%A��A��mA���A�|�A�S�A�G�A�A�A�=qA�;dA���A�dZA�l�A��A��FA��A���A�;dA�S�A���A�v�A��yA�l�A�JA�C�A��#A��RA�n�A���A�A��;A�"�A���A�9XA�G�A��A�|�A�/A�(�A��A�v�A��jA�K�A�A�A�A�A���A�1'A�XA��A��uA���A�ZA��A���A�G�A���A�$�A�G�A���A���A� �A~(�A{;dAyAv�yAt�yAsO�Ao��AnffAm�Ak�Aj��Aj$�Ail�Ag��AeG�Ad^5AchsAaA`jA_ƨA^��A]l�A\1'A[ƨAZ�HAY�AX-AW+AU�ATA�ASdZARbNAQ�AOl�AN�ANA�AM?}AM�AM%ALJAJ��AI�AG��AFv�AE�^AC��ACp�ACXAB�AA��AAp�A@�`A?�#A?+A>A=��A=
=A<��A<1A:�DA9��A8�`A8jA7+A6ffA5�A5�A4��A3|�A2��A2  A1�-A1?}A0Q�A/�A/�PA/A.VA-;dA,ffA,�A+��A+dZA*z�A)XA(��A(ĜA(M�A(bA'dZA&r�A%�A%��A%7LA$jA#�#A#�FA"ffA!�;A!/A ��A jA�PA�`AA�`A�A  A�AȴA^5A��A?}A�uA��AoA(�A�wAp�A(�AXAr�A7LA9XA��AVA�RA��AoA
�DA	��A	t�A��A�A��AoAJA�PA?}A��A-A��A|�A?}A  A��AS�A �9A V@��@���@��@��@��+@�hs@���@��/@�1@�v�@�-@�bN@�t�@�!@��-@��/@�j@�E�@�^@�D@��H@��@�D@�@�M�@�O�@�j@�t�@��y@���@ڇ+@��@���@�1@�=q@��@Դ9@�33@�(�@��H@�E�@��@�ȴ@��@�O�@��@ȓu@�+@��@ă@��m@�l�@�@��-@��j@�|�@��y@��7@��@�|�@�@�Z@��@�V@�J@�@��7@�G�@�(�@��H@��\@�J@�G�@�z�@�33@�~�@�p�@�Ĝ@�Q�@���@�n�@�=q@��#@�&�@���@�z�@���@�E�@�z�@��@�C�@���@��R@�ff@�@��7@��@�I�@��P@�K�@�~�@��@�%@���@�Q�@���@�"�@�+@�@���@���@�@�V@�Ĝ@�Z@���@�33@��y@��\@�-@��#@���@�/@���@�Q�@��@���@�\)@��y@��@���@�5?@��-@���@��m@���@�  @�9X@�I�@�Z@�Z@�Z@�r�@�1'@��w@�S�@��y@�~�@�5?@�J@�J@�{@��@�@���@�X@�V@���@��@��@�Z@�1'@��
@��@���@���@�v�@�ff@�^5@�E�@��T@���@�p�@���@��@��u@�Q�@�z�@�j@�I�@��@�l�@�dZ@�S�@�33@��R@�5?@�@���@�hs@�X@���@��m@�l�@�+@�;d@�t�@�  @�K�@�=q@���@���@��\@�@�J@���@�@���@�+@���@��R@���@�~�@��#@�p�@�7L@��@��`@�Q�@��@�r�@�Z@�9X@��@�b@�@�@|�@~�+@~@}�h@|�@|��@|z�@|1@{ƨ@{�F@{�F@{C�@z��@z��@z�\@z^5@z=q@y��@y��@y�7@yX@u�9@l�@du�@_s@X�.@Q��@LM@EX@?��@:_�@4'R@.�\@)rG@$�P@\�@�7@�@N<@�@
	@q�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A�dZA�bNA�bNA�VA�-A�$�A�$�A�"�A� �A� �A��A��A��A��A��A��A��A��A�{A�bA�JA�%A���A���A��`A�ƨA�C�A�\)A���A�{A�%A��A��mA���A�|�A�S�A�G�A�A�A�=qA�;dA���A�dZA�l�A��A��FA��A���A�;dA�S�A���A�v�A��yA�l�A�JA�C�A��#A��RA�n�A���A�A��;A�"�A���A�9XA�G�A��A�|�A�/A�(�A��A�v�A��jA�K�A�A�A�A�A���A�1'A�XA��A��uA���A�ZA��A���A�G�A���A�$�A�G�A���A���A� �A~(�A{;dAyAv�yAt�yAsO�Ao��AnffAm�Ak�Aj��Aj$�Ail�Ag��AeG�Ad^5AchsAaA`jA_ƨA^��A]l�A\1'A[ƨAZ�HAY�AX-AW+AU�ATA�ASdZARbNAQ�AOl�AN�ANA�AM?}AM�AM%ALJAJ��AI�AG��AFv�AE�^AC��ACp�ACXAB�AA��AAp�A@�`A?�#A?+A>A=��A=
=A<��A<1A:�DA9��A8�`A8jA7+A6ffA5�A5�A4��A3|�A2��A2  A1�-A1?}A0Q�A/�A/�PA/A.VA-;dA,ffA,�A+��A+dZA*z�A)XA(��A(ĜA(M�A(bA'dZA&r�A%�A%��A%7LA$jA#�#A#�FA"ffA!�;A!/A ��A jA�PA�`AA�`A�A  A�AȴA^5A��A?}A�uA��AoA(�A�wAp�A(�AXAr�A7LA9XA��AVA�RA��AoA
�DA	��A	t�A��A�A��AoAJA�PA?}A��A-A��A|�A?}A  A��AS�A �9A V@��@���@��@��@��+@�hs@���@��/@�1@�v�@�-@�bN@�t�@�!@��-@��/@�j@�E�@�^@�D@��H@��@�D@�@�M�@�O�@�j@�t�@��y@���@ڇ+@��@���@�1@�=q@��@Դ9@�33@�(�@��H@�E�@��@�ȴ@��@�O�@��@ȓu@�+@��@ă@��m@�l�@�@��-@��j@�|�@��y@��7@��@�|�@�@�Z@��@�V@�J@�@��7@�G�@�(�@��H@��\@�J@�G�@�z�@�33@�~�@�p�@�Ĝ@�Q�@���@�n�@�=q@��#@�&�@���@�z�@���@�E�@�z�@��@�C�@���@��R@�ff@�@��7@��@�I�@��P@�K�@�~�@��@�%@���@�Q�@���@�"�@�+@�@���@���@�@�V@�Ĝ@�Z@���@�33@��y@��\@�-@��#@���@�/@���@�Q�@��@���@�\)@��y@��@���@�5?@��-@���@��m@���@�  @�9X@�I�@�Z@�Z@�Z@�r�@�1'@��w@�S�@��y@�~�@�5?@�J@�J@�{@��@�@���@�X@�V@���@��@��@�Z@�1'@��
@��@���@���@�v�@�ff@�^5@�E�@��T@���@�p�@���@��@��u@�Q�@�z�@�j@�I�@��@�l�@�dZ@�S�@�33@��R@�5?@�@���@�hs@�X@���@��m@�l�@�+@�;d@�t�@�  @�K�@�=q@���@���@��\@�@�J@���@�@���@�+@���@��R@���@�~�@��#@�p�@�7L@��@��`@�Q�@��@�r�@�Z@�9X@��@�b@�@�@|�@~�+@~@}�h@|�@|��@|z�@|1@{ƨ@{�F@{�F@{C�@z��@z��@z�\@z^5@z=q@y��@y��@y�7G�O�@u�9@l�@du�@_s@X�.@Q��@LM@EX@?��@:_�@4'R@.�\@)rG@$�P@\�@�7@�@N<@�@
	@q�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBXBXBXBXBW
BW
BXBXBXBXBXBXBYBYBYBZB\)B`BBbNBe`Be`Be`BdZBdZBcTBbNBVB+BB�sB�/B��B��BĜBB��B��B��B��B�wB�^B�-B��B��B�VB�Bu�Bn�BbNBZBT�BL�BD�B>wB33B$�B5?B9XB-B!�B{B1B��B��B�B�NB��B��B��BɺBŢB�RB��B�7Bq�BcTBYBH�B:^B,B"�B!�B�B�BoBJBB
�B
�B
�/B
��B
r�B
T�B
?}B
-B
�B
PB	�B	�yB	�5B	��B	ɺB	B	�jB	�B	��B	��B	�hB	�1B	�%B	�1B	�%B	�+B	�B	� B	x�B	o�B	e`B	_;B	VB	K�B	E�B	A�B	?}B	2-B	.B	.B	.B	?}B	<jB	33B	'�B	�B	JB��B��B�B�B�B�yB�`B�`B�fB�fB�ZB�;B�)B�B�B�B��B��B��B��BɺBȴBǮBŢBĜB��B�jB�XB�RB�FB�9B�-B�'B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B�{B�oB�hB�VB�JB�=B�+B�B�B�B~�B{�Bx�Bt�Bo�Bl�BffBcTBaHB_;B]/B[#BYBVBR�BP�BN�BK�BH�BE�BB�B?}B;dB:^B8RB6FB5?B33B2-B1'B/B.B,B+B(�B'�B&�B%�B$�B#�B"�B!�B�B�B�B�B�B�B�B�B�B�B�B�B{BuBhBbB\B\BVBPBVBbB\B\BVBPBJBDBDB
=B	7B1B1B%BBBBBBBBBBBBBB%B+B1B	7B	7B1B	7BJBJBJBJBPBVB\BhBhBoBuBuBoB�B�B�B�B�B�B�B�B �B �B!�B"�B#�B'�B'�B+B-B-B1'B33B33B49B6FB7LB8RB:^B>wBE�BH�BL�BO�BO�BS�BXBW
BXBZB^5B`BBcTBgmBjBk�Bo�Br�Bu�B{�B�B�B�B�B�%B�+B�+B�1B�7B�=B�PB�\B�hB�oB��B��B��B��B��B��B��B��B��B�B�B�B�-B�9B�?B�^B�qB�}B��BŢBȴB��B��B��B��B��B��B�
B�B�B�/B�NB�ZB�fB�yB�B�B�B�B�B�B��B��B��B	  B	B	B	B	+B	DB	JB	VB	\B	hB	uB	�B	�B	�B	�B	 �B	"�B	#�B	$�B	&�B	'�B	(�B	+B	0!B	2-B	2-B	1'B	2-B	33B	5?B	9XB	=qB	=qB	:^B	<jB	@�B	H�B	L�B	I�B	J�B	M�B	S�B	W
B	W
B	ZB	^5B	aHB	`BB	bNB	bNB	dZB	iyB	l�B	o�B	r�B	v�B	w�B	y�B	y�B	y�B	z�B	{�B	� B	�B	�B	�+B	�1B	�7B	�DB	�JB	�JB	�JB	�VB	�bB	�hB	�oB	�uB	�uB	��B	��B	��B	��B	�sB	��B	�TB	�B	�	B
 �B
�B
�B
(�B
-CB
6�B
>wB
EmB
I�B
R:B
XEB
]�B
b�B
i_B
l�B
r|11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   BW�BW�BW�BW�BV�BV�BW�BW�BW�BW�BW�BW�BX�BX�BX�BZB\B`$Bb1BeABeABe?Bd:Bd:Bc:Bb0BU�B*�B �B�RB�B��B˪B�{B�oB�gB�iB�fB�dB�WB�?B�B��B��B�4B��Bu�BnyBb0BY�BT�BL�BDzB>TB3B$�B5B91B,�B!�B[BB��B��B�pB�*BϻB˥BʝBəB�}B�0B��B�Bq�Bc2BX�BH�B::B+�B"�B!�B�BmBKB(B�B
�B
�aB
�
B
��B
r�B
T�B
?YB
,�B
�B
,B	�B	�WB	�B	��B	ɕB	�iB	�GB	��B	��B	�vB	�EB	�B	��B	�B	�B	�B	��B	�B	x�B	owB	e:B	_B	U�B	K�B	E|B	AfB	?WB	2B	-�B	-�B	-�B	?VB	<BB	3B	'�B	rB	!B��B��B�}B�rB�jB�RB�7B�9B�?B�>B�5B�B�B��B��B��B��B��BϸBͮBɒBȌBǅB�}B�vB�\B�@B�2B�(B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�yB�jB�XB�QB�GB�@B�-B�"B�B�B��B��B��B~�B{�Bx�Bt�BouBlaBf=Bc+Ba"B_B]BZ�BX�BU�BR�BP�BN�BK�BH�BEwBBcB?VB;=B:3B8)B6B5B3B2B0�B.�B-�B+�B*�B(�B'�B&�B%�B$�B#�B"�B!�B�B�B�B�B{BvBqBdBgBqBiBbBPBHB=B6B0B1B,B$B.B7B0B3B+B#BBBB
B	
BBB�B�B�B�B�B�B�B�B�B�B �B�B�B�B�B BB	B	BB	
BB BBB$B(B-B;B<BBBHBHBBBaBmByB{B~B�BzB�B �B �B!�B"�B#�B'�B'�B*�B,�B,�B0�B3B3B4B6B7B8#B:1B>IBEuBH�BL�BO�BO�BS�BW�BV�BW�BY�B^B`Bc'BgABjQBkWBoqBr�Bu�B{�B��B��B��B��B��B��B��B�B�B�B�$B�-B�9B�BB�TB�XB�oB��B��B��B��B��B��B��B��B��B��B�
B�B�1B�BB�SB�WB�tBȆB̠BΪBиBѽB��B��B��B��B��B��B�B�+B�7B�JB�XB�]B�`B�jB�rB�{B��B��B��B��B	�B	�B	�B	�B	B	B	)B	.B	<B	IB	kB	xB	wB	~B	 �B	"�B	#�B	$�B	&�B	'�B	(�B	*�B	/�B	1�B	1�B	0�B	2B	3B	5B	9)B	=AB	=AB	:0B	<<B	@SB	H�B	L�B	I�B	J�B	M�B	S�B	V�B	V�B	Y�B	^B	aB	`B	bB	bB	d+B	iIB	l\B	opB	r�B	v�B	w�B	y�B	y�B	y�B	z�B	{�B	�B	��B	��B	��B	�B	�	B	�B	�B	�B	�B	�'B	�3B	�:B	�>B	�GB	�FB	�RB	�^B	�^G�O�B	�EB	��B	�&B	�tB	��B
 oB
nB
�B
(^B
-B
6B
>IB
E@B
I�B
RB
XB
]�B
b�B
i1B
l�B
rP11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.35 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             201906040952592019060409525920190604095259  AO  ARCAADJP                                                                    20181121041519    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121041519  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121041519  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604095259  IP                  G�O�G�O�G�O�                