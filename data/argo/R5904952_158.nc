CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:40Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  A�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Hd   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  J   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  P�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  WX   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  Y   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  _�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  aT   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  g�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  n�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  pH   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  v�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  x�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  <   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    l   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �l   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �l   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �l   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005190540  20181005190540  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @����:1   @����	@0�O�;dZ�cʗ�O�;1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @�  @�  A   A   A@  Aa��A���A�  A�  A�  A�  A�  A���A���B   B  B  B  B��B(  B0  B8  B@ffBH  BP  BX  B`  Bh  Bo��Bx  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  C �C�C  C  C  C	�fC  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C7�fC:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf�Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv�Cx  Cz  C|�C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C�  C�  C��C�  C�  C�  C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C��3C��3C�  C�  C�  C��C��C��C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  D   D y�D  D� DfD�fD  D� D  D� D  D�fDfD�fDfD� D��D� D	  D	� D
  D
� D
��D� D  Dy�D��D� D  D� DfD�fDfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD�fD  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#y�D$  D$�fD%  D%� D&fD&� D'  D'� D(  D(� D)fD)�fD*fD*� D+  D+y�D,  D,�fD-  D-y�D.  D.� D/  D/� D0  D0�fD1fD1� D2  D2� D2��D3y�D3��D4y�D4��D5� D6  D6y�D6��D7� D8fD8� D9  D9� D:  D:� D;  D;y�D<  D<� D=  D=� D>  D>� D?  D?� D@fD@�fDA  DA�fDB  DB� DC  DC� DD  DD� DD��DEy�DE��DFy�DG  DG� DHfDH� DI  DI� DJ  DJ� DK  DK� DL  DLy�DM  DM� DN  DN�fDO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  D�J�D�s�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@ǮA�
A#�
AC�
Aep�A��RA��A��A��A��A��A�RA�RB ��B��B��B��B �]B(��B0��B8��BA\)BH��BP��BX��B`��Bh��Bp�]Bx��B�z�B�z�B�z�B�z�B��B��B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B��B�z�B�z�C WCWC=qC=qC=qC
#�C=qC=qC=qC=qC=qC=qC=qC=qC=qC=qC =qC"=qC$=qC&=qC(=qC*=qC,=qC.=qC0=qC2=qC4=qC6=qC8#�C:=qC<=qC>=qC@=qCB=qCD=qCF=qCH=qCJ=qCL=qCN=qCP=qCR=qCT=qCV=qCX=qCZ=qC\=qC^=qC`=qCb=qCd=qCfWCh=qCj=qCl=qCn=qCp=qCr=qCt=qCvWCx=qCz=qC|WC~=qC��C��C��C��C��C��C��C��C��C�+�C��C��C��C��C�+�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�+�C��C��C�+�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�+�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�+�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�+�C��C��C��C��C��C��C��C��C��C��C��C�+�C�+�C��C��C��C��C��C��C�+�C�+�C�+�C��C��C��C��C��C�+�C��C��C��C��C��C��C��C��C��C��D \D ��D\D�\D�D��D\D�\D\D�\D\D��D�D��D�D�\D�D�\D	\D	�\D
\D
�\D�D�\D\D��D�D�\D\D�\D�D��D�D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D�D��D\D�\D\D�\D\D�\D\D�\D \D �\D!\D!�\D"\D"�\D#\D#��D$\D$��D%\D%�\D&�D&�\D'\D'�\D(\D(�\D)�D)��D*�D*�\D+\D+��D,\D,��D-\D-��D.\D.�\D/\D/�\D0\D0��D1�D1�\D2\D2�\D3�D3��D4�D4��D5�D5�\D6\D6��D7�D7�\D8�D8�\D9\D9�\D:\D:�\D;\D;��D<\D<�\D=\D=�\D>\D>�\D?\D?�\D@�D@��DA\DA��DB\DB�\DC\DC�\DD\DD�\DE�DE��DF�DF��DG\DG�\DH�DH�\DI\DI�\DJ\DJ�\DK\DK�\DL\DL��DM\DM�\DN\DN��DO\DO�\DP\DP�\DQ\DQ�\DR\DR�\DS\DS�\DT\D�R�D�{311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A�  A�  A���A�A�A�A�A�A�%A�A�A�  A���A��A��yA��/A���A���Aѩ�Aџ�AэPA�E�A��#AН�A�VA���Aϟ�A��/A΁A�E�A��A�JA�A���A���A��A��A�ĜA���A���Aͺ^A͸RAͲ-Aͩ�A���A�A��A��TA��HA���A�~�A�l�A�XA�O�A�VA��
A�ĜA̾wA̮A̛�A�v�A�VA�t�A� �A�|�Aŗ�A�S�A�A�I�A� �A�n�A�|�A���A��HA�\)A��A��TA�;dA�I�A�&�A��9A���A��/A�A�A���A�ȴA���A��^A��9A���A��wA�^5A���A���A��uA�5?A��A��A���A��A��A��A� �A��RA��uA�oA� �A�G�A��A�&�A�+A|ZAz��Ax�RAs��Ap  AnQ�AjJAf�DAb�A_�-A]��AZ(�AW&�AU"�AP�AO�ANALn�AG��AE��AA�mA?O�A>ZA=��A<�yA;�;A:��A:�A8 �A6v�A5XA4�A3p�A2v�A0��A-A+K�A)O�A'A%�7A$A"9XA r�A+A�RAĜA��AAO�AK�A+AA�Ap�A�A��A�FA+A�uA9XA��A?}A33A+A�/A��A+A�A�-AȴAdZA
Q�A	��A	hsA�DA��A5?A��A�
A��A�;A�mA�mA �A\)A �R@���@�/@��P@�-@�X@�&�@���@�\)@�ȴ@��7@�+@�=q@���@��@�Q�@��@�G�@�-@��@�l�@��T@�7L@�1@���@�n�@��@� �@��;@�33@�~�@ݩ�@�  @�-@�%@؛�@�I�@ם�@�
=@���@�@�?}@�Z@�l�@Ѻ^@��@���@�(�@υ@�;d@Η�@���@��@�Z@�A�@� �@�\)@���@���@�@�/@�r�@�b@Ǯ@�l�@�;d@�~�@š�@�?}@��@��/@�Ĝ@ă@���@�l�@�;d@�33@§�@�^5@���@�X@��9@�z�@���@��+@�ff@�M�@��@�/@�j@�(�@��m@�33@��y@��@��@���@�9X@�S�@�C�@�ȴ@��@�&�@��@�r�@�Q�@�ƨ@�dZ@�
=@���@�~�@�J@�X@��m@�C�@�
=@���@�v�@�J@�hs@��`@���@�Ĝ@��u@�(�@�b@��m@��w@���@���@��@�l�@�\)@��@���@���@�J@�x�@�G�@���@���@�Q�@��
@��@�t�@�S�@�;d@�@��R@�~�@�ff@�M�@�=q@��T@��@�p�@�V@��u@�I�@��P@�dZ@�\)@�C�@��+@�E�@�$�@��#@���@��@�`B@�&�@���@��@��@�bN@�A�@�b@�1@��
@��P@�\)@�+@��H@�n�@�E�@�@���@���@��`@�1@��m@��w@�\)@�o@��@���@�$�@���@�7L@�r�@� �@���@���@��P@�dZ@�\)@�+@���@��@��@�ȴ@���@��\@��@��#@��@��@���@���@��u@��@�r�@�bN@�b@��w@��@�\)@z�@d|�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A���A�  A�  A���A�A�A�A�A�A�%A�A�A�  A���A��A��yA��/A���A���Aѩ�Aџ�AэPA�E�A��#AН�A�VA���Aϟ�A��/A΁A�E�A��A�JA�A���A���A��A��A�ĜA���A���Aͺ^A͸RAͲ-Aͩ�A���A�A��A��TA��HA���A�~�A�l�A�XA�O�A�VA��
A�ĜA̾wA̮A̛�A�v�A�VA�t�A� �A�|�Aŗ�A�S�A�A�I�A� �A�n�A�|�A���A��HA�\)A��A��TA�;dA�I�A�&�A��9A���A��/A�A�A���A�ȴA���A��^A��9A���A��wA�^5A���A���A��uA�5?A��A��A���A��A��A��A� �A��RA��uA�oA� �A�G�A��A�&�A�+A|ZAz��Ax�RAs��Ap  AnQ�AjJAf�DAb�A_�-A]��AZ(�AW&�AU"�AP�AO�ANALn�AG��AE��AA�mA?O�A>ZA=��A<�yA;�;A:��A:�A8 �A6v�A5XA4�A3p�A2v�A0��A-A+K�A)O�A'A%�7A$A"9XA r�A+A�RAĜA��AAO�AK�A+AA�Ap�A�A��A�FA+A�uA9XA��A?}A33A+A�/A��A+A�A�-AȴAdZA
Q�A	��A	hsA�DA��A5?A��A�
A��A�;A�mA�mA �A\)A �R@���@�/@��P@�-@�X@�&�@���@�\)@�ȴ@��7@�+@�=q@���@��@�Q�@��@�G�@�-@��@�l�@��T@�7L@�1@���@�n�@��@� �@��;@�33@�~�@ݩ�@�  @�-@�%@؛�@�I�@ם�@�
=@���@�@�?}@�Z@�l�@Ѻ^@��@���@�(�@υ@�;d@Η�@���@��@�Z@�A�@� �@�\)@���@���@�@�/@�r�@�b@Ǯ@�l�@�;d@�~�@š�@�?}@��@��/@�Ĝ@ă@���@�l�@�;d@�33@§�@�^5@���@�X@��9@�z�@���@��+@�ff@�M�@��@�/@�j@�(�@��m@�33@��y@��@��@���@�9X@�S�@�C�@�ȴ@��@�&�@��@�r�@�Q�@�ƨ@�dZ@�
=@���@�~�@�J@�X@��m@�C�@�
=@���@�v�@�J@�hs@��`@���@�Ĝ@��u@�(�@�b@��m@��w@���@���@��@�l�@�\)@��@���@���@�J@�x�@�G�@���@���@�Q�@��
@��@�t�@�S�@�;d@�@��R@�~�@�ff@�M�@�=q@��T@��@�p�@�V@��u@�I�@��P@�dZ@�\)@�C�@��+@�E�@�$�@��#@���@��@�`B@�&�@���@��@��@�bN@�A�@�b@�1@��
@��P@�\)@�+@��H@�n�@�E�@�@���@���@��`@�1@��m@��w@�\)@�o@��@���@�$�@���@�7L@�r�@� �@���@���@��P@�dZ@�\)@�+@���@��@��@�ȴ@���@��\@��@��#@��@��@���@���@��u@��@�r�@�bN@�b@��w@��@�\)@z�@d|�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B-B-B-B-B-B-B-B-B-B-B.B-B0!B33B6FB8RB;dB>wB@�BF�BH�BI�BJ�BQ�BW
B[#B\)B\)B^5B_;BZBW
BVBVBT�BT�BVB[#Bw�B�JB�uB��B��B��B��B��B�'B�'B�!B�-B�'B��B��B��B��B��B��B��B��B��B��B��B�?B��B�B�;B�mB��B�BF�BZBe`BiyBn�Br�Bp�Bp�Bs�B� B�B�Bz�BgmBXBP�BE�B-BoB��B�`B��B�B��B�B\)B(�B\B
�B
�B
�sB
�HB
�B
ŢB
��B
�%B
o�B
O�B
5?B
�B	��B	�`B	�B	�dB	�'B	�3B	��B	�B	z�B	gmB	T�B	C�B	33B	'�B	�B	PB	B�B�mB�`B�BB��B��BǮB��B�^B�9B�B��B�B�9B�RB�LB�FB�3B�'B�!B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B��B��B��B��B��B�B�B�!B�B�'B�!B�B�B��B��B�!B�-B�}B��B��BBBƨBɺBB��B�wB�wBȴB��B�B�B�/B�TB�HB�NB�NB�NB�ZB�yB�B�yB�sB�yB�yB�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B	B	B	+B	
=B	PB	bB	�B	�B	�B	�B	�B	�B	&�B	+B	+B	-B	49B	5?B	7LB	;dB	<jB	>wB	A�B	C�B	C�B	D�B	D�B	E�B	D�B	F�B	I�B	M�B	R�B	S�B	VB	VB	XB	YB	\)B	^5B	`BB	aHB	bNB	e`B	ffB	iyB	k�B	l�B	l�B	m�B	m�B	o�B	q�B	s�B	x�B	x�B	|�B	� B	�B	�B	�%B	�+B	�1B	�=B	�=B	�JB	�PB	�PB	�PB	�bB	�hB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�-B	�3B	�9B	�9B	�9B	�9B	�?B	�FB	�FB	�FB	�LB	�RB	�XB	�XB	�XB	�^B	�jB	�qB	�qB	�wB	�}B	�}B	��B	��B	B	B	ƨB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�#B	�#B	�)B	�5B	�;B	�BB	�HB	�HB	�HB	�BB	�HB	�NB	�NB	�HB	�HB	�NB	�`B	�fB	�fB	�fB	�`B	�fB	�fB	�mB	�mB	�mB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B
'�B
2�22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   B-B-B-B-B-B-B-B-B-B-B.B-B0!B33B6FB8RB;dB>wB@�BF�BH�BI�BJ�BQ�BW
B[#B\)B\)B^5B_;BZBW
BVBVBT�BT�BVB[#Bw�B�JB�uB��B��B��B��B��B�'B�'B�!B�-B�'B��B��B��B��B��B��B��B��B��B��B��B�?B��B�B�;B�mB��B�BF�BZBe`BiyBn�Br�Bp�Bp�Bs�B� B�B�Bz�BgmBXBP�BE�B-BoB��B�`B��B�B��B�B\)B(�B\B
�B
�B
�sB
�HB
�B
ŢB
��B
�%B
o�B
O�B
5?B
�B	��B	�`B	�B	�dB	�'B	�3B	��B	�B	z�B	gmB	T�B	C�B	33B	'�B	�B	PB	B�B�mB�`B�BB��B��BǮB��B�^B�9B�B��B�B�9B�RB�LB�FB�3B�'B�!B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B��B��B��B��B��B�B�B�!B�B�'B�!B�B�B��B��B�!B�-B�}B��B��BBBƨBɺBB��B�wB�wBȴB��B�B�B�/B�TB�HB�NB�NB�NB�ZB�yB�B�yB�sB�yB�yB�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B	B	B	+B	
=B	PB	bB	�B	�B	�B	�B	�B	�B	&�B	+B	+B	-B	49B	5?B	7LB	;dB	<jB	>wB	A�B	C�B	C�B	D�B	D�B	E�B	D�B	F�B	I�B	M�B	R�B	S�B	VB	VB	XB	YB	\)B	^5B	`BB	aHB	bNB	e`B	ffB	iyB	k�B	l�B	l�B	m�B	m�B	o�B	q�B	s�B	x�B	x�B	|�B	� B	�B	�B	�%B	�+B	�1B	�=B	�=B	�JB	�PB	�PB	�PB	�bB	�hB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�-B	�3B	�9B	�9B	�9B	�9B	�?B	�FB	�FB	�FB	�LB	�RB	�XB	�XB	�XB	�^B	�jB	�qB	�qB	�wB	�}B	�}B	��B	��B	B	B	ƨB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�#B	�#B	�)B	�5B	�;B	�BB	�HB	�HB	�HB	�BB	�HB	�NB	�NB	�HB	�HB	�NB	�`B	�fB	�fB	�fB	�`B	�fB	�fB	�mB	�mB	�mB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B
'�B
2�22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.24 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190540                              AO  ARCAADJP                                                                    20181005190540    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190540  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190540  QCF$                G�O�G�O�G�O�8000            