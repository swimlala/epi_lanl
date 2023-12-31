CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:27Z creation      
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
resolution        =���   axis      Z        l  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  B�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J$   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  L    TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  Sl   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Z�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  \�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  d    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  e�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  mh   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  t�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  v�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �d   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �$   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �4   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �8   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �<   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �@Argo profile    3.1 1.2 19500101000000  20181005190527  20181005190527  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               eA   AO  6431                            2B  A   APEX                            7466                            062512                          846 @��'�>�1   @��(���@1�\(��c}����1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      eA   A   A   @�ff@�33A   A   A@  A`  A�  A�  A�  A�  A���A�  A�  A�  A�33B  B  B  B   B'��B0  B8ffB@  BG��BP  BXffB`  BhffBpffBx  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�33B�  B���C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6�C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C]�fC_�fCb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C��C��C��C��C�  C��3C��3C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��C��C��C�  C��C�  C��3C��3C�  C�  C�  C��3C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C��C��C�  C�  D   D �fDfD� D��D� D  D�fD  D� D  D� D  D� D  D� D  D� D��D	y�D	��D
� D  D� D  D� D��D� D  D� D  D� D��D� D  D� D  D�fD  D� D  D� DfD� DfD� D��D� D  D� D  D� DfD� D��D� DfD� D  D� D  D�fD  Dy�D��D � D!fD!�fD"fD"� D#  D#� D$  D$y�D%  D%�fD&  D&y�D&��D'� D(  D(� D)  D)� D*  D*� D+  D+� D+��D,y�D-  D-� D.  D.� D/fD/�fD0fD0� D1  D1� D1��D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7y�D8  D8� D9  D9� D9��D:y�D;  D;� D<  D<� D=  D=y�D>  D>� D>��D?� D@fD@� DA  DA� DA��DBy�DC  DC� DDfDD�fDE  DE� DF  DF�fDG  DG� DH  DH� DIfDI� DJ  DJ� DKfDK�fDL  DL� DMfDM� DM��DN� DO  DO� DPfDP� DP��DQ� DR  DR�fDS  DS� DT  DT� DU  DU� DU��DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[�fD\  D\y�D]  D]�fD^  D^�fD_  D_� D`  D`� D`��Da� Db  Db� Dc  Dc� Dc��Dd� De  Dey�Df  Df� Dg  Dg�fDhfDh� Di  Di� DjfDj� Dk  Dky�Dk��Dl� Dm  Dmy�Dm��Dn� Do  Do�fDpfDp� Dq  Dqy�Dq��Dr� DsfDs� Ds��Dt� Du  Du� Dv  Dv� Dw  Dw� DwٚDy��D�=�D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�z@��GA�
A#�
AC�
Ac�
A��A��A��A��A¸RA��A��A��B �\B��B��B��B ��B(�]B0��B9\)B@��BH�]BP��BY\)B`��Bi\)Bq\)Bx��B�z�B�z�B�z�B�z�B�z�B�z�B�G�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�BخB�z�B�z�B�z�B�z�B�z�B�z�B��B�z�B�G�C =qC=qC=qC=qC=qC
=qC=qC=qC=qC=qC=qC=qC=qC=qC=qC=qC =qC"=qC$=qC&=qC(=qC*=qC,=qC.=qC0=qC2=qC4=qC6WC8=qC:=qC<=qC>=qC@=qCB=qCD=qCF=qCH=qCJ=qCL=qCN=qCP=qCR=qCT=qCV=qCX=qCZ=qC\=qC^#�C`#�Cb=qCd=qCf=qCh=qCj=qCl=qCn=qCp=qCr=qCt=qCv=qCx=qCz=qC|=qC~=qC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�+�C�+�C�+�C�+�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�+�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�+�C��C��C��C��C��C��C��C�+�C��C��C��C�+�C�+�C�+�C��C�+�C��C��C��C��C��C��C��C��C��C�+�C��C��C��C��C��C��C��C��C�+�C�+�C��C��D \D ��D�D�\D�D�\D\D��D\D�\D\D�\D\D�\D\D�\D\D�\D	�D	��D
�D
�\D\D�\D\D�\D�D�\D\D�\D\D�\D�D�\D\D�\D\D��D\D�\D\D�\D�D�\D�D�\D�D�\D\D�\D\D�\D�D�\D�D�\D�D�\D\D�\D\D��D\D��D �D �\D!�D!��D"�D"�\D#\D#�\D$\D$��D%\D%��D&\D&��D'�D'�\D(\D(�\D)\D)�\D*\D*�\D+\D+�\D,�D,��D-\D-�\D.\D.�\D/�D/��D0�D0�\D1\D1�\D2�D2�\D3\D3�\D4\D4�\D5\D5�\D6\D6�\D7\D7��D8\D8�\D9\D9�\D:�D:��D;\D;�\D<\D<�\D=\D=��D>\D>�\D?�D?�\D@�D@�\DA\DA�\DB�DB��DC\DC�\DD�DD��DE\DE�\DF\DF��DG\DG�\DH\DH�\DI�DI�\DJ\DJ�\DK�DK��DL\DL�\DM�DM�\DN�DN�\DO\DO�\DP�DP�\DQ�DQ�\DR\DR��DS\DS�\DT\DT�\DU\DU�\DV�DV�\DW\DW�\DX\DX�\DY\DY�\DZ\DZ�\D[\D[��D\\D\��D]\D]��D^\D^��D_\D_�\D`\D`�\Da�Da�\Db\Db�\Dc\Dc�\Dd�Dd�\De\De��Df\Df�\Dg\Dg��Dh�Dh�\Di\Di�\Dj�Dj�\Dk\Dk��Dl�Dl�\Dm\Dm��Dn�Dn�\Do\Do��Dp�Dp�\Dq\Dq��Dr�Dr�\Ds�Ds�\Dt�Dt�\Du\Du�\Dv\Dv�\Dw\Dw�\Dw��Dy�RD�EqD���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A���A���A���A���A��
A��
A��A��A��A��
A��
A��A��A��A��A��A��A��A��#A��#A��#A��#A��#A��#A��#A��#A��A��#A��A��
A���Aۇ+A�"�Aڕ�A�`BAه+A��A���A��
A��A͛�A�bA���A�5?Aȉ7AǛ�A���A�bNA���A�I�A�1A���Aº^A��PA���A�G�A���A�33A��7A�jA�A�r�A�5?A�A��/A��`A��A�ffA��A��!A��wA��A��hA��9A���A�(�A�$�A��#A���A��uA���A�1'A�"�A� �A��A���A�z�A��PA�5?A��DA�E�A�E�A���A�;dA�  A��#A�|�A�XA�  A��-A�A�1'A��7A�"�A�A���A�bA�=qA�x�A|�yAx�/AtJAm��Ai+Ag�PAf��Aep�AbĜA_7LA[oAY�PAV�yAU&�AOdZAL1'AK�AJr�AHVAG�AFȴAD��ACG�AAhsA@{A?oA=��A<��A;��A;�A9�TA89XA5C�A4Q�A3K�A2^5A1K�A05?A/�A.=qA,��A+��A)�TA);dA(�jA(  A'�hA&�`A%ƨA$��A#t�A �9A%AAt�A��AE�A�AhsAVAx�A��A�A  A��A�uA��AȴA j@�V@���@���@��`@���@�G�@�+@�-@��@�ff@�"�@�"�@��;@�$�@�J@���@�bN@���@�?}@@��@�{@�S�@ܴ9@���@۝�@�l�@ߕ�@�9X@�j@�I�@��m@���@�A�@�C�@ڏ\@ڰ!@�@�j@�;d@�{@�`B@���@ԛ�@���@ӕ�@ӕ�@�@�Z@ϕ�@ϕ�@��;@���@���@�%@���@�|�@���@�?}@�%@ȓu@�9X@ǍP@��@�E�@���@�x�@�`B@ļj@�33@�ȴ@�@�~�@�v�@�V@���@�hs@�(�@�ƨ@��@�l�@�K�@�@�v�@��@���@��7@�hs@�?}@�j@��m@�C�@�v�@�=q@�{@��T@��^@��-@��-@���@�G�@���@�9X@��P@�l�@�\)@�\)@�\)@�;d@��@��H@�5?@��@��7@�x�@�x�@��@��j@���@� �@�  @���@��;@�C�@��T@�G�@�/@���@�z�@� �@�  @��@�C�@��@�ff@�5?@��@�x�@��@��7@��@�/@�z�@��@�;d@��@�v�@��7@�V@���@��@���@�r�@�I�@�(�@�1@��;@�ƨ@�1'@���@��m@���@�K�@�"�@�33@��H@��+@�n�@�v�@�{@��-@��-@���@��h@��@�X@�?}@��`@��P@��@���@�I�@�ƨ@��@�o@��@��!@�E�@���@�/@�&�@�%@��@��m@��@�;d@�ff@��@��7@��h@��@���@��u@��@���@�t�@�C�@��y@��\@���@��\@�~�@�E�@�=q@�5?@���@���@���@�x�@�X@�?}@��@��@��9@��m@���@��P@�|�@�33@���@�ff@�V@�E�@�{@��h@�O�@��j@�j@�1@�dZ@�@�ȴ@���@�M�@�{@��T@��^@��-@��h@���@���@��@���@��F@��F@���@�;d@��@�5?@���@��-@���@��`@�r�@�9X@���@��@��;@��w@�S�@��@���@��\@���@�v�@�-@��@�O�@��`@��/@��/@��9@�A�@��
@��P@�o@��R@�~�@�n�@�V@�E�@�=q@�$�@�c@zJ�@hG1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A���A���A���A���A��
A��
A��A��A��A��
A��
A��A��A��A��A��A��A��A��#A��#A��#A��#A��#A��#A��#A��#A��A��#A��A��
A���Aۇ+A�"�Aڕ�A�`BAه+A��A���A��
A��A͛�A�bA���A�5?Aȉ7AǛ�A���A�bNA���A�I�A�1A���Aº^A��PA���A�G�A���A�33A��7A�jA�A�r�A�5?A�A��/A��`A��A�ffA��A��!A��wA��A��hA��9A���A�(�A�$�A��#A���A��uA���A�1'A�"�A� �A��A���A�z�A��PA�5?A��DA�E�A�E�A���A�;dA�  A��#A�|�A�XA�  A��-A�A�1'A��7A�"�A�A���A�bA�=qA�x�A|�yAx�/AtJAm��Ai+Ag�PAf��Aep�AbĜA_7LA[oAY�PAV�yAU&�AOdZAL1'AK�AJr�AHVAG�AFȴAD��ACG�AAhsA@{A?oA=��A<��A;��A;�A9�TA89XA5C�A4Q�A3K�A2^5A1K�A05?A/�A.=qA,��A+��A)�TA);dA(�jA(  A'�hA&�`A%ƨA$��A#t�A �9A%AAt�A��AE�A�AhsAVAx�A��A�A  A��A�uA��AȴA j@�V@���@���@��`@���@�G�@�+@�-@��@�ff@�"�@�"�@��;@�$�@�J@���@�bN@���@�?}@@��@�{@�S�@ܴ9@���@۝�@�l�@ߕ�@�9X@�j@�I�@��m@���@�A�@�C�@ڏ\@ڰ!@�@�j@�;d@�{@�`B@���@ԛ�@���@ӕ�@ӕ�@�@�Z@ϕ�@ϕ�@��;@���@���@�%@���@�|�@���@�?}@�%@ȓu@�9X@ǍP@��@�E�@���@�x�@�`B@ļj@�33@�ȴ@�@�~�@�v�@�V@���@�hs@�(�@�ƨ@��@�l�@�K�@�@�v�@��@���@��7@�hs@�?}@�j@��m@�C�@�v�@�=q@�{@��T@��^@��-@��-@���@�G�@���@�9X@��P@�l�@�\)@�\)@�\)@�;d@��@��H@�5?@��@��7@�x�@�x�@��@��j@���@� �@�  @���@��;@�C�@��T@�G�@�/@���@�z�@� �@�  @��@�C�@��@�ff@�5?@��@�x�@��@��7@��@�/@�z�@��@�;d@��@�v�@��7@�V@���@��@���@�r�@�I�@�(�@�1@��;@�ƨ@�1'@���@��m@���@�K�@�"�@�33@��H@��+@�n�@�v�@�{@��-@��-@���@��h@��@�X@�?}@��`@��P@��@���@�I�@�ƨ@��@�o@��@��!@�E�@���@�/@�&�@�%@��@��m@��@�;d@�ff@��@��7@��h@��@���@��u@��@���@�t�@�C�@��y@��\@���@��\@�~�@�E�@�=q@�5?@���@���@���@�x�@�X@�?}@��@��@��9@��m@���@��P@�|�@�33@���@�ff@�V@�E�@�{@��h@�O�@��j@�j@�1@�dZ@�@�ȴ@���@�M�@�{@��T@��^@��-@��h@���@���@��@���@��F@��F@���@�;d@��@�5?@���@��-@���@��`@�r�@�9X@���@��@��;@��w@�S�@��@���@��\@���@�v�@�-@��@�O�@��`@��/@��/@��9@�A�@��
@��P@�o@��R@�~�@�n�@�V@�E�@�=q@�$�@�c@zJ�@hG1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
%�B
%�B
%�B
%�B
%�B
$�B
%�B
$�B
%�B
%�B
%�B
%�B
'�B
&�B
&�B
(�B
)�B
)�B
)�B
)�B
)�B
+B
,B
,B
-B
.B
0!B
1'B
1'B
2-B
33B
8RB
>wB
B�B
Q�B
T�B
ZB
s�B
w�B
v�B
�B
t�B
r�B
s�B
�B
�yB%B�B.B;dBP�B~�B��B��B�9B�^B�jBɺB�ZB��B��B  BoB�B�B�BVB��B��BDB;dBI�BK�BQ�BZB[#BXBG�BC�BI�BO�B �BhBB��B\B+B��B�B�FB��Bm�BA�B(�B{B\BVB+BB
�B
�
B
ȴB
��B
�RB
��B
��B
�7B
[#B
>wB
-B

=B	�fB	�}B	��B	�B	w�B	s�B	iyB	`BB	S�B	A�B	9XB	-B	 �B	DB��B��B��B�B�B�yB�BB�B��B��B��BȴBŢBÖB��B�qB�XB�?B�3B�'B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�'B�)BɺBĜBĜB��B�jB�-B��B��B��B��B��B�B�B��B��B��B�{B��B��B�XBB��B�}B�dB��B��B�)B�-B�{B�\B�VB��B��BŢB��B��B��B��B��B�B�
B�B�)B�5B�/B�/B�/B�/B�/B�/B�5B�;B�;B�BB�NB�fB�yB�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B	  B	  B	B	
=B	JB	PB	PB	PB	PB	VB	bB	�B	�B	�B	�B	�B	�B	�B	"�B	$�B	$�B	$�B	%�B	,B	0!B	7LB	<jB	=qB	>wB	?}B	A�B	A�B	A�B	A�B	C�B	D�B	G�B	L�B	O�B	Q�B	R�B	R�B	S�B	XB	[#B	^5B	dZB	gmB	iyB	iyB	k�B	m�B	m�B	p�B	p�B	p�B	p�B	s�B	z�B	}�B	}�B	� B	�B	�B	�B	�B	�B	�%B	�1B	�7B	�7B	�DB	�JB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�RB	�XB	�^B	�dB	��B	ĜB	ŢB	ƨB	ŢB	ŢB	ŢB	ƨB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�#B	�5B	�;B	�;B	�;B	�;B	�BB	�HB	�NB	�NB	�NB	�NB	�ZB	�`B	�`B	�fB	�fB	�mB	�mB	�mB	�mB	�mB	�sB	�yB	�yB	�yB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
B
%B
+B
1B
	7B
DB
JB
JB
JB
PB
VB
PB
JB
JB
JB
JB
JB
JB
DB
DB
DB
DB
PB
VB
PB
PB
VB
VB
VB
VB
VB
VB
VB
VB
bB
\B
\B
\B
\B
bB
\B
oB
�B
B
)�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 B
%�B
%�B
%�B
%�B
%�B
$�B
%�B
$�B
%�B
%�B
%�B
%�B
'�B
&�B
&�B
(�B
)�B
)�B
)�B
)�B
)�B
+B
,B
,B
-B
.B
0!B
1'B
1'B
2-B
33B
8RB
>wB
B�B
Q�B
T�B
ZB
s�B
w�B
v�B
�B
t�B
r�B
s�B
�B
�yB%B�B.B;dBP�B~�B��B��B�9B�^B�jBɺB�ZB��B��B  BoB�B�B�BVB��B��BDB;dBI�BK�BQ�BZB[#BXBG�BC�BI�BO�B �BhBB��B\B+B��B�B�FB��Bm�BA�B(�B{B\BVB+BB
�B
�
B
ȴB
��B
�RB
��B
��B
�7B
[#B
>wB
-B

=B	�fB	�}B	��B	�B	w�B	s�B	iyB	`BB	S�B	A�B	9XB	-B	 �B	DB��B��B��B�B�B�yB�BB�B��B��B��BȴBŢBÖB��B�qB�XB�?B�3B�'B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�'B�)BɺBĜBĜB��B�jB�-B��B��B��B��B��B�B�B��B��B��B�{B��B��B�XBB��B�}B�dB��B��B�)B�-B�{B�\B�VB��B��BŢB��B��B��B��B��B�B�
B�B�)B�5B�/B�/B�/B�/B�/B�/B�5B�;B�;B�BB�NB�fB�yB�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B	  B	  B	B	
=B	JB	PB	PB	PB	PB	VB	bB	�B	�B	�B	�B	�B	�B	�B	"�B	$�B	$�B	$�B	%�B	,B	0!B	7LB	<jB	=qB	>wB	?}B	A�B	A�B	A�B	A�B	C�B	D�B	G�B	L�B	O�B	Q�B	R�B	R�B	S�B	XB	[#B	^5B	dZB	gmB	iyB	iyB	k�B	m�B	m�B	p�B	p�B	p�B	p�B	s�B	z�B	}�B	}�B	� B	�B	�B	�B	�B	�B	�%B	�1B	�7B	�7B	�DB	�JB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�RB	�XB	�^B	�dB	��B	ĜB	ŢB	ƨB	ŢB	ŢB	ŢB	ƨB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�#B	�5B	�;B	�;B	�;B	�;B	�BB	�HB	�NB	�NB	�NB	�NB	�ZB	�`B	�`B	�fB	�fB	�mB	�mB	�mB	�mB	�mB	�sB	�yB	�yB	�yB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
B
%B
+B
1B
	7B
DB
JB
JB
JB
PB
VB
PB
JB
JB
JB
JB
JB
JB
DB
DB
DB
DB
PB
VB
PB
PB
VB
VB
VB
VB
VB
VB
VB
VB
bB
\B
\B
\B
\B
bB
\B
oB
�B
B
)�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.24 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190527                              AO  ARCAADJP                                                                    20181005190527    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190527  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190527  QCF$                G�O�G�O�G�O�8000            