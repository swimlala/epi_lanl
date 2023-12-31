CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2017-10-16T00:03:15Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         (Matthew Alkire, University of Washington      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
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
_FillValue                    �,Argo profile    3.1 1.2 19500101000000  20171016000315  20190604095308  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4055                            2C  D   APEX                            5374                            041511                          846 @�-��p�{1   @�-����@:������c!V�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�ff@�  A   AffA>ffA`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BHffBP  BX  B`  Bh  Bp  Bx  B�  B�  B�33B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B���B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtY�Dy�D��D�V�D���D�ǮD�D�P�D��qD�ʏD�=D�.fD�ED�׮D��D�7
D�u�D�׮D�D�@RD�h�D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@�33A��A$  AD  Ae��A���A���A���A���A���A���A���A���BffB	ffBffBffB!ffB)ffB1ffB9ffBAffBI��BQffBYffBaffBiffBqffByffB��3B��3B��fB�� B�L�B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3Bĳ3Bȳ3B̳3Bг3BԳ3Bس3B��fB��fB� B�3B�3B�3B��3B��3B��3C Y�CY�CY�CY�CY�C
Y�CY�CY�CY�CY�CY�CY�CY�CY�CY�CY�C Y�C"Y�C$Y�C&Y�C(Y�C*Y�C,Y�C.Y�C0Y�C2Y�C4Y�C6Y�C8Y�C:Y�C<Y�C>Y�C@Y�CBY�CDY�CFY�CHY�CJY�CLY�CNY�CPY�CRY�CTY�CVY�CXY�CZY�C\Y�C^Y�C`Y�CbY�CdY�CfY�ChY�CjY�ClY�CnY�CpY�CrY�CtY�CvY�CxY�CzY�C|Y�C~Y�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�D fD �fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD	fD	�fD
fD
�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD��DfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD fD �fD!fD!�fD"fD"�fD#fD#�fD$fD$�fD%fD%�fD&fD&�fD'fD'�fD(fD(�fD)fD)�fD*fD*�fD+fD+�fD,fD,�fD-fD-�fD.fD.�fD/fD/�fD0fD0�fD1fD1�fD2fD2�fD3fD3�fD4fD4�fD5fD5�fD6fD6�fD7fD7�fD8fD8�fD9fD9�fD:fD:�fD;fD;�fD<fD<�fD=fD=�fD>fD>�fD?fD?�fD@fD@�fDAfDA�fDBfDB�fDCfDC�fDDfDD�fDEfDE�fDFfDF�fDGfDG�fDHfDH�fDIfDI�fDJfDJ�fDKfDK�fDLfDL�fDMfDM�fDNfDN�fDOfDO�fDPfDP�fDQfDQ�fDRfDR�fDSfDS�fDTfDT�fDUfDU�fDVfDV�fDWfDW�fDXfDX�fDYfDY�fDZfDZ�fD[fD[�fD\fD\�fD]fD]�fD^fD^�fD_fD_�fD`fD`�fDafDa�fDbfDb�fDcfDc�fDdfDd�fDefDe�fDffDf�fDgfDg�fDhfDh�fDifDi�fDjfDj�fDkfDk�fDlfDl�fDmfDm�fDnfDn�fDofDo�fDpfDp�fDqfDq�fDrfDr�fDsfDs�fDtfDtp Dy�zD�!�D�a�D���D���D�GD�[�D���D���D�pD�9�D�PRD���D�
D�B=Dڀ�D���D�RD�K�D�t)D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�jA�l�A�jA�jA�jA�l�A�n�A�n�A�p�A�p�A�r�A�v�A�v�A�z�A�x�A�t�A�v�A�v�A�r�A�t�A�v�A�|�A�z�AՑhA��HA�ĜA�  A��A�1'A׋DAח�A׃A�r�A�jAѡ�A�A���A��;A�9XA��A��mA�C�A�1'A���A���A��A��A���A��!A�C�A��PA���A��A��9A�JA���A��`A�?}A�ȴA��A���A�VA���A�
=A�7LA��RA��9A��A��A��FA��A���A�\)A�ffA��+A�?}A���A�A��!A���A�r�A��9A�jA���A���A�~�A�l�A�~�A�Q�A�v�A���A���A���A�VA���A��^A�Q�A���A��A�/A�;A~��A}dZA{�Ay�Ay33Aw�Av  Arz�Aot�Ak7LAj5?Ai�7Ah�9AhJAg�AfZAf(�Af�AeƨAe�Acl�Ab(�AaC�A_G�A^��A]t�A\�+A[�A[��AZ��AY�AXbNAVjAU�hAR�AQx�AP��AN�HAL�+AKO�AJv�AG��AEG�AB�!AAl�A?��A=A< �A:Q�A7�7A6��A5�^A41A3G�A2�9A2r�A2M�A1�TA1/A01'A/l�A.�A,v�A*VA)`BA)\)A(ĜA)33A(ĜA(�/A)"�A*{A*{A)�-A(��A'\)A&M�A%;dA$�9A#�A"��A"�A!x�A �HA bAAQ�A�A�A^5AS�A��A�!AZA�#AXAĜAAbA�FA��A7LA(�A|�A%Az�AƨA/A�9A�+A�^A�9Az�AQ�A5?A��A	t�A�DAZA�A\)A�!Ap�A�AĜA�uA�
A�`AAG�A 5?@�S�@���@�@���@� �@���@�I�@�K�@��R@�O�@�bN@�1'@�@�`B@�r�@�l�@�{@���@�9@��@�?}@�9@�33@��@�v�@��@ޗ�@�-@�Ĝ@ۥ�@�;d@�S�@��@�  @��@��@�z�@�  @϶F@ΰ!@�@�x�@��`@˝�@�?}@�1@ƸR@Ł@�dZ@��@�A�@��
@�|�@�+@�v�@��T@���@�A�@��@��@���@��@���@��+@�E�@�$�@�J@��u@�dZ@�S�@�C�@�33@���@��@��7@�%@��j@��D@�A�@��w@��!@�V@���@���@��w@�K�@���@��H@��\@���@�7L@�I�@���@�l�@��\@�$�@�@�?}@�Z@��;@�C�@�@�~�@��#@�/@�I�@�A�@���@�"�@�ȴ@�ff@�I�@��@�ƨ@���@�ȴ@��R@���@�E�@���@��D@�Q�@��
@��y@�~�@�V@�E�@�ff@��@�Ĝ@�5?@���@�hs@�X@���@�Ĝ@��u@��u@���@�z�@�r�@�Z@�A�@�ƨ@�l�@�33@�"�@��H@�ff@�J@�J@�J@�@���@���@��@���@�?}@��/@��j@��9@��@��@���@��@�Q�@� �@��P@���@��@��@���@��@���@�bN@�A�@�9X@��m@��;@��m@�1@��;@��w@���@��P@��@�\)@���@�n�@���@�X@�Ĝ@�1'@��
@��@�dZ@��@���@��\@�=q@���@�hs@�7L@��@��@��D@�r�@�Z@�(�@��@+@~��@}�T@}?}@|z�@|1@{�F@{dZ@z��@z�\@zn�@zJ@zJ@y��@y��@yX@xA�@w�;@w��@w+@w�P@w�P@v�+@v�+@w\)@v��@v{@u@u/@t�@t�@t�@t�/@t�@t��@t��@t�@u/@t�@tj@t(�@t1@s�m@s�F@st�@s�@ra|@iu�@a��@[iD@SZ�@L֡@E�N@?dZ@9+�@5*0@/��@+�&@&�x@!�7@-w@�Y@�@J�@
d�@� @C�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A�jA�l�A�jA�jA�jA�l�A�n�A�n�A�p�A�p�A�r�A�v�A�v�A�z�A�x�A�t�A�v�A�v�A�r�A�t�A�v�A�|�A�z�AՑhA��HA�ĜA�  A��A�1'A׋DAח�A׃A�r�A�jAѡ�A�A���A��;A�9XA��A��mA�C�A�1'A���A���A��A��A���A��!A�C�A��PA���A��A��9A�JA���A��`A�?}A�ȴA��A���A�VA���A�
=A�7LA��RA��9A��A��A��FA��A���A�\)A�ffA��+A�?}A���A�A��!A���A�r�A��9A�jA���A���A�~�A�l�A�~�A�Q�A�v�A���A���A���A�VA���A��^A�Q�A���A��A�/A�;A~��A}dZA{�Ay�Ay33Aw�Av  Arz�Aot�Ak7LAj5?Ai�7Ah�9AhJAg�AfZAf(�Af�AeƨAe�Acl�Ab(�AaC�A_G�A^��A]t�A\�+A[�A[��AZ��AY�AXbNAVjAU�hAR�AQx�AP��AN�HAL�+AKO�AJv�AG��AEG�AB�!AAl�A?��A=A< �A:Q�A7�7A6��A5�^A41A3G�A2�9A2r�A2M�A1�TA1/A01'A/l�A.�A,v�A*VA)`BA)\)A(ĜA)33A(ĜA(�/A)"�A*{A*{A)�-A(��A'\)A&M�A%;dA$�9A#�A"��A"�A!x�A �HA bAAQ�A�A�A^5AS�A��A�!AZA�#AXAĜAAbA�FA��A7LA(�A|�A%Az�AƨA/A�9A�+A�^A�9Az�AQ�A5?A��A	t�A�DAZA�A\)A�!Ap�A�AĜA�uA�
A�`AAG�A 5?@�S�@���@�@���@� �@���@�I�@�K�@��R@�O�@�bN@�1'@�@�`B@�r�@�l�@�{@���@�9@��@�?}@�9@�33@��@�v�@��@ޗ�@�-@�Ĝ@ۥ�@�;d@�S�@��@�  @��@��@�z�@�  @϶F@ΰ!@�@�x�@��`@˝�@�?}@�1@ƸR@Ł@�dZ@��@�A�@��
@�|�@�+@�v�@��T@���@�A�@��@��@���@��@���@��+@�E�@�$�@�J@��u@�dZ@�S�@�C�@�33@���@��@��7@�%@��j@��D@�A�@��w@��!@�V@���@���@��w@�K�@���@��H@��\@���@�7L@�I�@���@�l�@��\@�$�@�@�?}@�Z@��;@�C�@�@�~�@��#@�/@�I�@�A�@���@�"�@�ȴ@�ff@�I�@��@�ƨ@���@�ȴ@��R@���@�E�@���@��D@�Q�@��
@��y@�~�@�V@�E�@�ff@��@�Ĝ@�5?@���@�hs@�X@���@�Ĝ@��u@��u@���@�z�@�r�@�Z@�A�@�ƨ@�l�@�33@�"�@��H@�ff@�J@�J@�J@�@���@���@��@���@�?}@��/@��j@��9@��@��@���@��@�Q�@� �@��P@���@��@��@���@��@���@�bN@�A�@�9X@��m@��;@��m@�1@��;@��w@���@��P@��@�\)@���@�n�@���@�X@�Ĝ@�1'@��
@��@�dZ@��@���@��\@�=q@���@�hs@�7L@��@��@��D@�r�@�Z@�(�@��@+@~��@}�T@}?}@|z�@|1@{�F@{dZ@z��@z�\@zn�@zJ@zJ@y��@y��@yX@xA�@w�;@w��@w+@w�P@w�P@v�+@v�+@w\)@v��@v{@u@u/@t�@t�@t�@t�/@t�@t��@t��@t�@u/@t�@tj@t(�@t1@s�m@s�F@st�G�O�@ra|@iu�@a��@[iD@SZ�@L֡@E�N@?dZ@9+�@5*0@/��@+�&@&�x@!�7@-w@�Y@�@J�@
d�@� @C�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBG�BG�BG�BG�BG�BH�BH�BH�BH�BH�BH�BI�BI�BI�BH�BH�BH�BH�BG�BG�BH�BJ�BI�BQ�Bs�B�NBB�BN�B��B�ZB�B%B��B�B�B��B��B��B�{B��B�{B�oB�1B|�Bv�Br�Bv�Bu�Bq�Bx�Bu�Bq�BjB\)BN�B=qB;dB8RB33B"�BhBB��B�NB��BɺB��B��B��B��B��B�9B��B��B��B��B�JBz�Bp�BdZB\)BXBW
BW
BXBXBG�B&�B�BPB
��B
�B
�yB
�BB
�
B
��B
ȴB
�qB
�-B
��B
��B
�VB
�B
y�B
s�B
hsB
VB
33B
JB	�B	��B	ÖB	�jB	�RB	�RB	�RB	�LB	�XB	�wB	��B	ĜB	�RB	�B	��B	��B	��B	�VB	�DB	��B	�hB	�+B	{�B	hsB	\)B	?}B	0!B	#�B	bB��B�B�;BB��B��B�DB|�Bl�BgmBVBD�BB�B=qB:^B8RB<jBC�BK�BR�BQ�BVBS�BQ�BI�B=qB>wBJ�BL�BdZBiyBp�B~�B��B�B�B��B��B��B��B��B�bB�JB�=B�%B�B�B~�B|�B{�By�Bw�Bu�Bt�Bs�Bq�Bo�Bm�BiyBe`BbNB`BBZBQ�BM�BM�BK�BH�BF�BD�BD�BC�BB�BB�BA�B@�B>wB:^B7LB6FB6FB6FB5?B2-B2-B2-B1'B0!B/B/B.B.B-B-B-B,B,B,B)�B)�B)�B(�B,B.B-B,B+B)�B)�B(�B(�B.B)�B'�B$�B �B�B�B{BuBhB\BuB�BPB1B%B+B1BDBDBDBVBbBbBhBoB�B�B�B�B�B�B!�B"�B"�B"�B$�B$�B&�B'�B+B-B-B-B2-B5?B6FB7LB6FB:^B?}B?}B?}B?}B?}BC�BE�BG�BH�BI�BJ�BK�BM�BM�BM�BP�BVBXBYBYBZB^5BaHBffBjBn�Bp�Bp�Bq�Bs�Bw�Bx�Bz�B|�B~�B�B� B�B�B�B�B�B� B�B�%B�+B�+B�%B�%B�%B�%B�%B�1B�DB�\B�oB�uB�{B�{B��B�{B�oB��B��B��B��B��B��B��B�B�B�B�B�B�B�!B�-B�RB�^B�jB��BÖBɺB��B��B��B��B��B��B�
B�5B�BB�HB�ZB�ZB�`B�`B�sB�yB�yB�B�B�B�B�B�B��B��B��B��B��B	  B	B	%B	1B		7B	
=B	
=B	DB	JB	bB	uB	�B	�B	�B	!�B	"�B	#�B	&�B	(�B	)�B	+B	-B	.B	/B	/B	1'B	2-B	33B	33B	5?B	7LB	9XB	;dB	>wB	B�B	F�B	I�B	K�B	L�B	P�B	Q�B	R�B	T�B	VB	VB	W
B	XB	\)B	^5B	`BB	e`B	k�B	l�B	m�B	u�B	|�B	~�B	~�B	~�B	� B	�B	�B	�B	�B	�B	�%B	�=B	�JB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	�eB	�B	�IB	��B	��B
B
&2B
-CB
5�B
?�B
@�B
H�B
NB
S�B
YeB
a�B
iDB
oOB
tB
x�B
}"11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   BG�BG�BG�BG�BG�BH�BH�BH�BH�BH�BH�BI�BI�BI�BH�BH�BH�BH�BG�BG�BH�BJ�BI�BQ�Bs�B�2B�B�BN�B��B�@B�BB��B�B��B��B��B�fB�^B�aB�^B�QB�B|�Bv�Br�Bv�Bu�Bq�Bx�Bu�Bq�Bj_B\BN�B=RB;@B83B3B"�BIB�B��B�/BϾBəB��B��B��BʠB�gB�B��B��B��B��B�*Bz�Bp�Bd7B\BW�BV�BV�BW�BW�BG�B&�BdB-B
��B
��B
�VB
�B
��B
оB
ȔB
�MB
�B
��B
�nB
�0B
��B
y�B
s�B
hOB
U�B
3B
%B	��B	ˢB	�qB	�FB	�,B	�.B	�-B	�'B	�5B	�RB	ˡB	�vB	�-B	��B	��B	��B	�iB	�2B	�B	�jB	�CB	�B	{�B	hNB	\B	?XB	/�B	#�B	<B��B�hB�B�hB��B�bB�B|�BlbBgEBU�BDtBBhB=KB:8B8*B<BBCoBK�BR�BQ�BU�BS�BQ�BI�B=JB>NBJ�BL�Bd3BiQBp~B~�B��B��B��B��B��B��B�sB�_B�8B�"B�B��B��B��B~�B|�B{�By�Bw�Bu�Bt�Bs�Bq�BotBmjBiPBe8Bb&B`BY�BQ�BM�BM�BK�BH�BF}BDuBDoBCnBBfBBeBA]B@[B>PB:5B7#B6B6B6B5B2B2B2B0�B/�B.�B.�B-�B-�B,�B,�B,�B+�B+�B+�B)�B)�B)�B(�B+�B-�B,�B+�B*�B)�B)�B(�B(�B-�B)�B'�B$�B �B�BZBPBJB?B/BHB\B$BB�B�BBBBB)B6B4B?BBBWBYB`BaB{B�B!�B"�B"�B"�B$�B$�B&�B'�B*�B,�B,�B,�B1�B5B6B7B6B:1B?QB?NB?OB?PB?QBChBEuBG�BH�BI�BJ�BK�BM�BM�BM�BP�BU�BW�BX�BX�BY�B^BaBf4BjRBnlBpvBpxBq{Bs�Bw�Bx�Bz�B|�B~�B��B�B��B��B��B��B��B�B��B��B��B��B��B��B��B��B��B�B�B�.B�@B�HB�LB�LB�UB�OB�?B�dB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�#B�1B�<B�TB�hBɊBͥBέBϲBϳBѿB��B��B�B�B�B�+B�+B�0B�1B�FB�LB�KB�QB�YB�cB�oB�zB�B�B��B��B��B��B��B	�B	�B	B		B	
B	
B	B	B	1B	GB	YB	~B	�B	!�B	"�B	#�B	&�B	(�B	)�B	*�B	,�B	-�B	.�B	.�B	0�B	2 B	3B	3B	5B	7B	9)B	;6B	>MB	B_B	F{B	I�B	K�B	L�B	P�B	Q�B	R�B	T�B	U�B	U�B	V�B	W�B	[�B	^B	`B	e1B	kVB	l^B	mcB	u�B	|�B	~�B	~�B	~�B	�B	��B	��B	��B	��B	��B	��B	�B	�B	�MB	�UB	�eB	�rB	�xB	�wB	�B	��G�O�B	�9B	��B	�B	��B	��B
�B
&B
-B
5yB
?�B
@�B
H�B
M�B
S{B
Y6B
a�B
iB
o"B
s�B
x�B
|�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.35 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             201906040953082019060409530820190604095308  AO  ARCAADJP                                                                    20171016000315    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20171016000315  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20171016000315  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604095308  IP                  G�O�G�O�G�O�                