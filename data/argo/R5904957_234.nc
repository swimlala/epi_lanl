CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:48Z creation      
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
resolution        =���   axis      Z        p  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  B�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J,   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  L   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  Sx   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Z�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  \�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  d4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  f   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  m�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  t�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  v�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~<   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �4   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �D   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �H   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �X   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �\   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �`   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �dArgo profile    3.1 1.2 19500101000000  20181024140848  20181024140848  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6560                            2B  A   APEX                            7471                            062512                          846 @��$�yo1   @��%M��	@5�l�C��dl�C��1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @333@�  @�  A   A   A@  A`  A�  A���A�  A�  A�  A���A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B���B�  B�  B�  B�  B�  B���C�fC  C  C�C
  C  C  C  C  C  C  C  C�fC  C  C   C"  C$  C&  C(  C*  C+�fC.  C0  C2  C4  C5�fC8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CW�fCZ  C\  C^  C_�fCa�fCd  Cf�Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� DfD� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,y�D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2fD2� D3  D3� D4  D4� D5  D5� D6  D6� D7fD7�fD8  D8� D9  D9� D:  D:� D;fD;� D<  D<�fD=  D=� D>  D>� D?  D?�fD@fD@�fDA  DAy�DB  DB� DC  DC� DD  DD�fDE  DE� DF  DFy�DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DNy�DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DUy�DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  Dey�Df  Df� Dg  Dg� Dh  Dh� DifDi� Dj  Dj� Dk  Dk�fDl  Dl� DmfDm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� DvfDv�fDw  Dw� Dw�fDy�D�1�D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@B�\@��@ǮA�
A#�
AC�
Ac�
A��A��RA��A��A��AҸRA��A��B ��B��B��B��B ��B(��B0��B8��B@��BH��BP��BX��B`��Bh��Bp��Bx��B�z�B�z�B�z�B�G�B�z�B�z�B�z�B�z�B�z�C #�C#�C=qC=qCWC
=qC=qC=qC=qC=qC=qC=qC=qC#�C=qC=qC =qC"=qC$=qC&=qC(=qC*=qC,#�C.=qC0=qC2=qC4=qC6#�C8=qC:=qC<=qC>=qC@=qCB=qCD=qCF=qCH=qCJ=qCL=qCN=qCP=qCR=qCT=qCV=qCX#�CZ=qC\=qC^=qC`#�Cb#�Cd=qCfWCh=qCj=qCl=qCn=qCp=qCr=qCt=qCv=qCx=qCz=qC|=qC~=qC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�+�C�+�C��C��C��C��C��C��C��C��C��C�+�C�+�C��C��C��C��C��C��C��C��C��C��C��C�+�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D \D �\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D��D\D�\D	\D	�\D
\D
�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D�D�\D\D�\D\D�\D�D�\D�D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D \D �\D!\D!�\D"\D"�\D#\D#�\D$\D$�\D%\D%�\D&\D&�\D'\D'�\D(\D(�\D)\D)�\D*\D*�\D+\D+�\D,\D,��D-\D-�\D.\D.�\D/\D/�\D0\D0�\D1\D1�\D2�D2�\D3\D3�\D4\D4�\D5\D5�\D6\D6�\D7�D7��D8\D8�\D9\D9�\D:\D:�\D;�D;�\D<\D<��D=\D=�\D>\D>�\D?\D?��D@�D@��DA\DA��DB\DB�\DC\DC�\DD\DD��DE\DE�\DF\DF��DG\DG�\DH\DH�\DI\DI�\DJ\DJ�\DK\DK�\DL\DL�\DM\DM�\DN\DN��DO\DO�\DP\DP�\DQ\DQ�\DR\DR�\DS\DS�\DT\DT�\DU\DU��DV\DV�\DW\DW�\DX\DX�\DY\DY�\DZ\DZ�\D[\D[�\D\\D\�\D]\D]�\D^\D^�\D_\D_�\D`\D`�\Da\Da�\Db\Db�\Dc\Dc�\Dd\Dd�\De\De��Df\Df�\Dg\Dg�\Dh\Dh�\Di�Di�\Dj\Dj�\Dk\Dk��Dl\Dl�\Dm�Dm�\Dn\Dn�\Do\Do�\Dp\Dp�\Dq\Dq�\Dr\Dr�\Ds\Ds�\Dt\Dt�\Du\Du�\Dv�Dv��Dw\Dw�\Dw��Dy�pD�9HD���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A��A�"�A�"�A�$�A� �A�"�A�$�A�$�A���A���A��yA��TA��A���A���A���AǶFAǗ�AǓuAǉ7A�+A��A���AƼjAƼjA���A��TA��
A�v�A�K�A�&�A�-A��A�A�ȴA�M�A�hsA��wA�p�A�(�A��PA�1'A��A�ĜA�jA���A�?}A��9A���A�bNA��jA��mA�bA�t�A��FA�+A���A��7A�bA��A�A�(�A�n�A���A�l�A�7LA���A�-A���A�A�A�A�I�A���A���A��^A���A�JA�1A�;dA~�\A{�PAzA�Az  Ayp�Av�uAr��AmAj�Aj1Ai��Ai33AhE�Af��Ab�jAa�hA_&�AZ1'AX~�AV�/AU"�AOO�AL1'AK/AJ��AIK�AG`BAD�AC�wAC�FACdZAA��A@$�A?�^A?O�A>=qA=p�A;l�A9��A8�A8^5A7x�A5��A4~�A25?A.�/A-VA*ȴA)/A&�`A&ZA%��A#��A"�+A!�A!G�A ��A 5?A�AdZA?}A/AA��AA�AS�A��A�\AbA�hAQ�AC�A�/A�mA��A"�AZA��A��Ap�A�9A?}Ar�A��A��A�mA
��A�HA��A�`A(�A��A�A�AdZA33A"�A��A1A��A|�A Ĝ@�S�@���@�@��7@�\)@�V@�p�@�(�@�|�@�@���@��@�t�@�^@���@�u@�9X@�+@��T@���@�r�@߅@�r�@�~�@�^5@�~�@ڗ�@ڗ�@ڟ�@ڰ!@�M�@��T@�hs@��`@���@���@�
=@Ձ@�Z@��H@��T@��@�  @���@�|�@��@���@�@��@ΰ!@�n�@���@�`B@̋D@���@�S�@�n�@���@�hs@ȋD@� �@�|�@�"�@���@ƸR@�ff@��@�p�@�1'@Ý�@¸R@§�@���@��;@�dZ@�@��H@��\@�7L@�1@�dZ@�$�@�V@��@�5?@�Q�@��\@�Z@�  @��
@�|�@�dZ@���@�5?@�"�@��@�|�@���@�  @�(�@�  @��;@���@�l�@�C�@�ȴ@��T@��h@��@�b@�S�@�ȴ@�{@��-@�X@�/@��@�bN@�9X@� �@��@��@��@�1@�1@�(�@�r�@�Z@�dZ@��\@�M�@��@���@��7@�p�@�X@�`B@�x�@�@��^@���@�hs@��@���@�K�@��!@��R@�V@��@��^@���@�p�@�?}@���@��u@�|�@��H@���@�5?@�-@�=q@�=q@�M�@���@���@��\@�~�@�n�@�M�@��#@���@��@�o@���@��w@��w@�l�@��y@��#@��h@���@���@�bN@�ƨ@�K�@�33@�+@��@��@�v�@��@���@�V@��@��-@���@��^@��^@���@�hs@��@��`@�Ĝ@�Ĝ@�z�@��w@���@�\)@�K�@�33@�o@��@���@���@�v�@�E�@���@��^@�X@���@�dZ@�dZ@��
@� �@��w@�l�@�S�@�@���@�v�@�V@��#@���@�p�@��@�%@���@��`@�&�@�7L@�?}@�&�@��@��@��j@�z�@�j@�Z@�(�@���@�|�@��@��\@�J@�p�@���@���@�z�@�z�@�j@�  @��
@��F@���@�V@���@�x�@�/@�O�@�X@��@��`@�Ĝ@��D@�Z@���@��@���@���@��;@��@��w@��P@�S�@�33@�o@��R@�E�@���@��T@��z@w>�@fH�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A��A�"�A�"�A�$�A� �A�"�A�$�A�$�A���A���A��yA��TA��A���A���A���AǶFAǗ�AǓuAǉ7A�+A��A���AƼjAƼjA���A��TA��
A�v�A�K�A�&�A�-A��A�A�ȴA�M�A�hsA��wA�p�A�(�A��PA�1'A��A�ĜA�jA���A�?}A��9A���A�bNA��jA��mA�bA�t�A��FA�+A���A��7A�bA��A�A�(�A�n�A���A�l�A�7LA���A�-A���A�A�A�A�I�A���A���A��^A���A�JA�1A�;dA~�\A{�PAzA�Az  Ayp�Av�uAr��AmAj�Aj1Ai��Ai33AhE�Af��Ab�jAa�hA_&�AZ1'AX~�AV�/AU"�AOO�AL1'AK/AJ��AIK�AG`BAD�AC�wAC�FACdZAA��A@$�A?�^A?O�A>=qA=p�A;l�A9��A8�A8^5A7x�A5��A4~�A25?A.�/A-VA*ȴA)/A&�`A&ZA%��A#��A"�+A!�A!G�A ��A 5?A�AdZA?}A/AA��AA�AS�A��A�\AbA�hAQ�AC�A�/A�mA��A"�AZA��A��Ap�A�9A?}Ar�A��A��A�mA
��A�HA��A�`A(�A��A�A�AdZA33A"�A��A1A��A|�A Ĝ@�S�@���@�@��7@�\)@�V@�p�@�(�@�|�@�@���@��@�t�@�^@���@�u@�9X@�+@��T@���@�r�@߅@�r�@�~�@�^5@�~�@ڗ�@ڗ�@ڟ�@ڰ!@�M�@��T@�hs@��`@���@���@�
=@Ձ@�Z@��H@��T@��@�  @���@�|�@��@���@�@��@ΰ!@�n�@���@�`B@̋D@���@�S�@�n�@���@�hs@ȋD@� �@�|�@�"�@���@ƸR@�ff@��@�p�@�1'@Ý�@¸R@§�@���@��;@�dZ@�@��H@��\@�7L@�1@�dZ@�$�@�V@��@�5?@�Q�@��\@�Z@�  @��
@�|�@�dZ@���@�5?@�"�@��@�|�@���@�  @�(�@�  @��;@���@�l�@�C�@�ȴ@��T@��h@��@�b@�S�@�ȴ@�{@��-@�X@�/@��@�bN@�9X@� �@��@��@��@�1@�1@�(�@�r�@�Z@�dZ@��\@�M�@��@���@��7@�p�@�X@�`B@�x�@�@��^@���@�hs@��@���@�K�@��!@��R@�V@��@��^@���@�p�@�?}@���@��u@�|�@��H@���@�5?@�-@�=q@�=q@�M�@���@���@��\@�~�@�n�@�M�@��#@���@��@�o@���@��w@��w@�l�@��y@��#@��h@���@���@�bN@�ƨ@�K�@�33@�+@��@��@�v�@��@���@�V@��@��-@���@��^@��^@���@�hs@��@��`@�Ĝ@�Ĝ@�z�@��w@���@�\)@�K�@�33@�o@��@���@���@�v�@�E�@���@��^@�X@���@�dZ@�dZ@��
@� �@��w@�l�@�S�@�@���@�v�@�V@��#@���@�p�@��@�%@���@��`@�&�@�7L@�?}@�&�@��@��@��j@�z�@�j@�Z@�(�@���@�|�@��@��\@�J@�p�@���@���@�z�@�z�@�j@�  @��
@��F@���@�V@���@�x�@�/@�O�@�X@��@��`@�Ĝ@��D@�Z@���@��@���@���@��;@��@��w@��P@�S�@�33@�o@��R@�E�@���@��T@��z@w>�@fH�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B��B��B��B��B��B��B�-B�9B�dB�dB�XB�RB�LB�XB�dB�qBŢBɺB��B��B��B��B��B��B�B�)B�TB�yB�B��B�uB�{B�{B�hB�oB�7B~�Bm�B,B�B��B�dB�B��B��B��B�JB�By�Bp�BiyB`BBT�BJ�BK�B>wB.B&�B�B�B{B
=BB
��B
��B
�B
�B
�)B
�;B
�B
��B
B
�dB
�LB
�B
��B
�PB
s�B
ZB
@�B
,B
&�B
&�B
 �B
\B	��B	�B	ɺB	ÖB	��B	�jB	�3B	��B	�=B	~�B	n�B	Q�B	F�B	:^B	,B	uB	B	  B��B��B�B�yB�B�B�B�sB�TB�BB�/B�/B�#B�B��B��B��B��BĜB��B�^B�!B��B��B��B��B��B�uB�\B�=B�7B�7B�=B�=B�DB�DB�=B�7B�1B�+B�7B�=B�hB�bB�hB�bB�bB�hB�bB�\B�PB�JB�DB�DB�DB�=B�=B�7B�JB�=B�B~�Bx�Bp�BcTB^5B\)B[#B[#B^5B`BBaHBaHBaHB`BBdZBffBffBffBe`BcTBdZBbNBaHBaHB`BBbNBhsBjBbNB^5B`BBcTBe`BffBgmBiyBl�Bm�Bm�Bo�Br�Bs�Bs�Bt�Bu�Bu�Bu�Bv�By�By�By�Bz�By�Bz�B}�B}�B{�B|�B~�B�B�7B�PB��B��B��B��B��B��B��B�B�B�!B�'B�-B�'B�3B�3B�9B�FB�^B�dB�jB�wB��BĜBȴB��B��B��B�B�/B�5B�;B�HB�HB�NB�ZB�fB�mB�`B�ZB�NB�BB�5B�5B�;B�;B�;B�;B�NB�fB�mB�B��B��B	B		7B	\B	oB	{B	�B	�B	�B	�B	$�B	%�B	'�B	-B	/B	1'B	1'B	2-B	49B	6FB	A�B	B�B	C�B	D�B	E�B	E�B	E�B	F�B	G�B	J�B	T�B	YB	]/B	_;B	bNB	cTB	e`B	iyB	jB	k�B	o�B	t�B	x�B	|�B	�B	�B	�B	�B	� B	�B	�B	�+B	�DB	�JB	�VB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	�9B	�LB	�XB	�jB	�qB	�jB	�wB	��B	ĜB	ƨB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�#B	�#B	�)B	�/B	�/B	�/B	�5B	�5B	�5B	�;B	�BB	�HB	�NB	�TB	�ZB	�`B	�fB	�`B	�fB	�fB	�`B	�HB	�BB	�HB	�ZB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
1B
1B
	7B

=B
DB
JB
PB
PB
JB
JB
B
"�B
2-11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B��B��B��B��B��B��B��B�-B�9B�dB�dB�XB�RB�LB�XB�dB�qBŢBɺB��B��B��B��B��B��B�B�)B�TB�yB�B��B�uB�{B�{B�hB�oB�7B~�Bm�B,B�B��B�dB�B��B��B��B�JB�By�Bp�BiyB`BBT�BJ�BK�B>wB.B&�B�B�B{B
=BB
��B
��B
�B
�B
�)B
�;B
�B
��B
B
�dB
�LB
�B
��B
�PB
s�B
ZB
@�B
,B
&�B
&�B
 �B
\B	��B	�B	ɺB	ÖB	��B	�jB	�3B	��B	�=B	~�B	n�B	Q�B	F�B	:^B	,B	uB	B	  B��B��B�B�yB�B�B�B�sB�TB�BB�/B�/B�#B�B��B��B��B��BĜB��B�^B�!B��B��B��B��B��B�uB�\B�=B�7B�7B�=B�=B�DB�DB�=B�7B�1B�+B�7B�=B�hB�bB�hB�bB�bB�hB�bB�\B�PB�JB�DB�DB�DB�=B�=B�7B�JB�=B�B~�Bx�Bp�BcTB^5B\)B[#B[#B^5B`BBaHBaHBaHB`BBdZBffBffBffBe`BcTBdZBbNBaHBaHB`BBbNBhsBjBbNB^5B`BBcTBe`BffBgmBiyBl�Bm�Bm�Bo�Br�Bs�Bs�Bt�Bu�Bu�Bu�Bv�By�By�By�Bz�By�Bz�B}�B}�B{�B|�B~�B�B�7B�PB��B��B��B��B��B��B��B�B�B�!B�'B�-B�'B�3B�3B�9B�FB�^B�dB�jB�wB��BĜBȴB��B��B��B�B�/B�5B�;B�HB�HB�NB�ZB�fB�mB�`B�ZB�NB�BB�5B�5B�;B�;B�;B�;B�NB�fB�mB�B��B��B	B		7B	\B	oB	{B	�B	�B	�B	�B	$�B	%�B	'�B	-B	/B	1'B	1'B	2-B	49B	6FB	A�B	B�B	C�B	D�B	E�B	E�B	E�B	F�B	G�B	J�B	T�B	YB	]/B	_;B	bNB	cTB	e`B	iyB	jB	k�B	o�B	t�B	x�B	|�B	�B	�B	�B	�B	� B	�B	�B	�+B	�DB	�JB	�VB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	�9B	�LB	�XB	�jB	�qB	�jB	�wB	��B	ĜB	ƨB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�#B	�#B	�)B	�/B	�/B	�/B	�5B	�5B	�5B	�;B	�BB	�HB	�NB	�TB	�ZB	�`B	�fB	�`B	�fB	�fB	�`B	�HB	�BB	�HB	�ZB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
1B
1B
	7B

=B
DB
JB
PB
PB
JB
JB
B
"�B
2-11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.24 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140848                              AO  ARCAADJP                                                                    20181024140848    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140848  QCP$                G�O�G�O�G�O�F03E            AO  ARGQQCPL                                                                    20181024140848  QCF$                G�O�G�O�G�O�0               