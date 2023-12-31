CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:49Z creation      
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
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A0   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \P   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^@   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  g�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  wp   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y`   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �    SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �    SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �    SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �    HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �,   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �0   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �4   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �8   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �<   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �|   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181005190549  20181005190549  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @��iu�/�1   @��j  �@1��G�{�c���v�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @�33@�  @���AffA>ffA`  A�  A���A�  A�  A�  A�  A�  A�33B   B  B  B��B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�33B�  B���B�  B�  B�  C   C�C�C  C�fC	�fC�fC  C  C  C  C  C  C�C  C  C �C"  C$  C&  C(  C*  C+�fC.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ�CL�CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv�Cx�Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C��C��C��C�  C�  C��3C�  C�  C�  C��C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C��3C�  C��C�  C��3C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  D   D � DfD� D��Dy�D  Dy�D��Dy�D  D� DfD�fDfD� D  D� D	fD	� D	��D
� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D��D� D  D� D  D�fDfD� D  D� D  Dy�D��Dy�D  D� D  D� D  D� D  D� D��D� D  D� D   D y�D ��D!y�D"  D"� D"��D#� D$fD$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*fD*�fD+fD+� D,fD,� D,��D-� D.  D.� D/  D/� D/��D0y�D1  D1�fD2  D2y�D3  D3�fD4  D4� D4��D5y�D5��D6y�D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D;��D<y�D<��D=y�D>  D>� D?  D?� D@  D@�fDAfDA�fDB  DB�fDCfDC�fDD  DD� DEfDE�fDFfDF� DF��DGy�DH  DH� DI  DI�fDJfDJ� DJ��DK� DL  DLy�DM  DM� DN  DN� DOfDO�fDPfDP�fDQfDQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DV��DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^fD^� D^��D_� D`  D`� Da  Day�Da��Db� Dc  Dc� Dd  Dd�fDe  De� DffDf� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk�fDl  Dly�Dl��Dm� DnfDn� Do  Do� Do��Dpy�Dq  Dq� Dr  Dry�Ds  Ds� Ds��Dty�Du  Duy�Dv  Dv�fDw  Dwy�Dyh�D�\1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��G@ǮA=qA"=pAB=pAc�
A��A��RA��A��A��A��A��A��B ��B��B��B�]B ��B(��B0��B8��B@��BH��BP��BX��B`��Bh��Bp��Bx��B�z�B�z�B�z�B�G�B�G�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�BȮB̮B�z�B�z�B�z�B�z�B�z�B�z�B�B�z�B�G�B�z�B�z�B�z�C =qCWCWC=qC#�C
#�C#�C=qC=qC=qC=qC=qC=qCWC=qC=qC WC"=qC$=qC&=qC(=qC*=qC,#�C.=qC0=qC2=qC4=qC6=qC8=qC:=qC<=qC>=qC@=qCB=qCD=qCF=qCH=qCJWCLWCN=qCP=qCR=qCT=qCV=qCX=qCZ=qC\=qC^=qC`=qCb=qCd=qCf=qCh=qCj=qCl=qCn=qCp=qCr=qCt=qCvWCxWCz=qC|=qC~=qC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�+�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�+�C�+�C��C��C��C��C��C��C��C��C��C��C�+�C�+�C�+�C�+�C��C��C��C��C��C��C�+�C��C��C��C��C��C��C��C��C��C��C�+�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�+�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D \D �\D�D�\D�D��D\D��D�D��D\D�\D�D��D�D�\D\D�\D	�D	�\D
�D
�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D�D�\D\D�\D�D�\D\D�\D\D��D�D�\D\D�\D\D��D�D��D\D�\D\D�\D\D�\D\D�\D�D�\D\D�\D \D ��D!�D!��D"\D"�\D#�D#�\D$�D$�\D%\D%�\D&\D&�\D'\D'�\D(\D(�\D)\D)�\D*�D*��D+�D+�\D,�D,�\D-�D-�\D.\D.�\D/\D/�\D0�D0��D1\D1��D2\D2��D3\D3��D4\D4�\D5�D5��D6�D6��D7\D7�\D8\D8�\D9\D9�\D:\D:�\D;\D;�\D<�D<��D=�D=��D>\D>�\D?\D?�\D@\D@��DA�DA��DB\DB��DC�DC��DD\DD�\DE�DE��DF�DF�\DG�DG��DH\DH�\DI\DI��DJ�DJ�\DK�DK�\DL\DL��DM\DM�\DN\DN�\DO�DO��DP�DP��DQ�DQ�\DR\DR�\DS\DS�\DT\DT�\DU\DU�\DV\DV�\DW�DW�\DX\DX�\DY\DY�\DZ\DZ�\D[\D[�\D\\D\�\D]\D]�\D^�D^�\D_�D_�\D`\D`�\Da\Da��Db�Db�\Dc\Dc�\Dd\Dd��De\De�\Df�Df�\Dg\Dg�\Dh\Dh�\Di\Di�\Dj\Dj�\Dk\Dk��Dl\Dl��Dm�Dm�\Dn�Dn�\Do\Do�\Dp�Dp��Dq\Dq�\Dr\Dr��Ds\Ds�\Dt�Dt��Du\Du��Dv\Dv��Dw\Dw��DyxRD�'
1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AƧ�AƬAƮAƮAƴ9A���A��#A�A�\)A�VAȰ!Aȧ�Aȩ�AȶFA���A���A�VA�/A�E�A�Q�A�M�A�I�A�I�A�E�A�C�A�C�A�A�A�A�A�;dA�7LA�-A�(�A��A��mA��/Aȩ�AȁA�S�A�A�A�?}AƧ�Aƙ�Aƛ�AƇ+A�z�AƧ�A���AƇ+A�I�A��A���A��A�S�A��`A�{A�`BA��HA�ZA²-ADA��
A���A�oA�^5A���A�`BA�1'A���A���A��9A���A�7LA���A�ZA��`A���A�K�A�  A��RA�M�A�z�A���A�A�bNA�K�A���A��A��A��hA���A�{A���A�
=A�G�A��mA�x�A�A�bA��!A�p�A�-A�bA�\)A�A�A���A��A�ZA��mA��A�l�A|n�Ay�Ax��AxZAwAt��Ao��AnM�Am�#Am�PAmO�Al�!AiG�AdZAa��A\�AX�AU�ASXAR��AM��AH��AG��AG�AF�`AE?}AB�DA>�jA<VA;�FA;"�A7�^A5��A4I�A2�HA1�A/�A-33A,bNA+�A+��A+`BA*��A)"�A( �A&�9A$�/A!�
A �jA�mA/AXA��A�A�HA�AG�A�A  A�AS�A�;AĜA��A�\AbNA�A��A��A��A-A��AXA
��A
~�A
E�A	t�A^5A\)A�!A1A$�A��A�9AQ�A��A"�AffAx�A ��@��-@� �@��@�J@��@�dZ@�@�9X@�+@�?}@�9@�(�@�ff@�r�@�o@��#@�h@��@�w@�o@�-@�&�@�1@�t�@��H@�!@�ff@�/@�bN@�l�@�n�@ܓu@�K�@�5?@ف@�Ĝ@��
@֗�@�/@ԓu@�  @��@��#@���@У�@�A�@υ@θR@�v�@�V@�=q@�J@��@��@�bN@��;@�l�@�"�@���@�^5@ɲ-@�7L@��@ȓu@�Q�@�b@��;@��;@�ƨ@�+@�~�@�@���@�@���@�@ŉ7@���@�j@�bN@�Z@�(�@��
@Å@�33@�33@�C�@�C�@�C�@�@§�@��T@���@��-@��7@�&�@���@�S�@��y@��\@�J@���@�X@��@��w@��@�dZ@�S�@�+@�o@��@�-@�7L@�V@��j@�Q�@�1'@�1@��@���@��F@���@�C�@��!@�~�@�~�@�{@���@��7@�x�@�&�@���@�Q�@�(�@��@�b@��m@�l�@���@�M�@��@��-@��7@�X@�G�@��@�z�@��w@�o@��!@���@�Q�@��P@�\)@�\)@�t�@�|�@�t�@�C�@���@�V@�5?@�$�@�{@���@�`B@�/@�Ĝ@�z�@�1'@���@��;@���@�dZ@�;d@�+@��@�
=@���@�ȴ@�v�@�E�@��@��-@��@�O�@�/@���@���@�9X@�(�@�b@��;@��@�
=@�@��h@�p�@�`B@�?}@��@��@��D@���@�+@�ff@�5?@�@��@��-@��7@�/@��`@��@��@�A�@���@�;d@�o@�o@�@��R@�E�@��@�O�@���@�Z@���@��@�33@�M�@��T@��7@�p�@�hs@�hs@�hs@�?}@�V@���@��@��/@���@�z�@�1'@�1@���@��;@��@�|�@�S�@�@���@��!@���@�v�@�n�@�ff@�V@�$�@�J@���@�?}@��@�z�@�Z@�1@���@�\)@��H@���@��\@��+@�v�@�E�@���@���@��`@� �@��m@��;@��;@�ƨ@��@�K�@���@��\@�v�@�^5@�M�@�E�@�E�@�=q@�$�@�_@�e1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111AƧ�AƬAƮAƮAƴ9A���A��#A�A�\)A�VAȰ!Aȧ�Aȩ�AȶFA���A���A�VA�/A�E�A�Q�A�M�A�I�A�I�A�E�A�C�A�C�A�A�A�A�A�;dA�7LA�-A�(�A��A��mA��/Aȩ�AȁA�S�A�A�A�?}AƧ�Aƙ�Aƛ�AƇ+A�z�AƧ�A���AƇ+A�I�A��A���A��A�S�A��`A�{A�`BA��HA�ZA²-ADA��
A���A�oA�^5A���A�`BA�1'A���A���A��9A���A�7LA���A�ZA��`A���A�K�A�  A��RA�M�A�z�A���A�A�bNA�K�A���A��A��A��hA���A�{A���A�
=A�G�A��mA�x�A�A�bA��!A�p�A�-A�bA�\)A�A�A���A��A�ZA��mA��A�l�A|n�Ay�Ax��AxZAwAt��Ao��AnM�Am�#Am�PAmO�Al�!AiG�AdZAa��A\�AX�AU�ASXAR��AM��AH��AG��AG�AF�`AE?}AB�DA>�jA<VA;�FA;"�A7�^A5��A4I�A2�HA1�A/�A-33A,bNA+�A+��A+`BA*��A)"�A( �A&�9A$�/A!�
A �jA�mA/AXA��A�A�HA�AG�A�A  A�AS�A�;AĜA��A�\AbNA�A��A��A��A-A��AXA
��A
~�A
E�A	t�A^5A\)A�!A1A$�A��A�9AQ�A��A"�AffAx�A ��@��-@� �@��@�J@��@�dZ@�@�9X@�+@�?}@�9@�(�@�ff@�r�@�o@��#@�h@��@�w@�o@�-@�&�@�1@�t�@��H@�!@�ff@�/@�bN@�l�@�n�@ܓu@�K�@�5?@ف@�Ĝ@��
@֗�@�/@ԓu@�  @��@��#@���@У�@�A�@υ@θR@�v�@�V@�=q@�J@��@��@�bN@��;@�l�@�"�@���@�^5@ɲ-@�7L@��@ȓu@�Q�@�b@��;@��;@�ƨ@�+@�~�@�@���@�@���@�@ŉ7@���@�j@�bN@�Z@�(�@��
@Å@�33@�33@�C�@�C�@�C�@�@§�@��T@���@��-@��7@�&�@���@�S�@��y@��\@�J@���@�X@��@��w@��@�dZ@�S�@�+@�o@��@�-@�7L@�V@��j@�Q�@�1'@�1@��@���@��F@���@�C�@��!@�~�@�~�@�{@���@��7@�x�@�&�@���@�Q�@�(�@��@�b@��m@�l�@���@�M�@��@��-@��7@�X@�G�@��@�z�@��w@�o@��!@���@�Q�@��P@�\)@�\)@�t�@�|�@�t�@�C�@���@�V@�5?@�$�@�{@���@�`B@�/@�Ĝ@�z�@�1'@���@��;@���@�dZ@�;d@�+@��@�
=@���@�ȴ@�v�@�E�@��@��-@��@�O�@�/@���@���@�9X@�(�@�b@��;@��@�
=@�@��h@�p�@�`B@�?}@��@��@��D@���@�+@�ff@�5?@�@��@��-@��7@�/@��`@��@��@�A�@���@�;d@�o@�o@�@��R@�E�@��@�O�@���@�Z@���@��@�33@�M�@��T@��7@�p�@�hs@�hs@�hs@�?}@�V@���@��@��/@���@�z�@�1'@�1@���@��;@��@�|�@�S�@�@���@��!@���@�v�@�n�@�ff@�V@�$�@�J@���@�?}@��@�z�@�Z@�1@���@�\)@��H@���@��\@��+@�v�@�E�@���@���@��`@� �@��m@��;@��;@�ƨ@��@�K�@���@��\@�v�@�^5@�M�@�E�@�E�@�=q@�$�@�_@�e1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	�B	�B	�B	�B	�B	��B	��B
PB
;dB
�oB
�;B
�BB
�HB
�`B
�B
��B
��B%BJB\B\B\B\BbBbBbBbBbB\B\B\BbB{B5?BQ�B[#B_;B^5BcTB>wB/B5?BN�B[#BiyB�B��B�hB�+B� B}�B�=B��B�qB��BŢBǮB��B�#B�B"�B49B>wBA�B2-B'�B#�B"�B5?BQ�BN�BR�BL�BJ�BN�B]/BbNBgmBhsBhsBe`BaHB\)BXBW
BQ�BG�B7LB1'B'�B�B��B�mB�/B��B�dB�B��B�Be`BT�B&�B
�XB
��B
�oB
}�B
XB
=qB
!�B	��B	ȴB	�9B	�B	��B	��B	�=B	p�B	n�B	p�B	n�B	o�B	p�B	iyB	Q�B	A�B	-B	�B	VB	%B	  B�B�HB�5B�#B�BƨB�9B��B��B��B��B��B��B��B��B��B��B�B�3B�?B�FB�FB�RB�dB�FB�B��B��B��B��B��B��B��B�B��B��B��B��B��B��B��B��B�B�B�B�B�B�'B�'B�'B�'B�FB�XB�FB�9B�3B�'B�!B�'B�-B�LB�}B��B�XB�RB�FB�FB�FB�LB�LB�qB�wB�qB��BÖBŢBȴB��B��B��B��B��B��B��B�B�)B�)B�5B�HB�NB�`B�`B�B�B�B�B�B�B�B��B��B��B��B	  B	B	B		7B	PB	hB	oB	uB	�B	�B	�B	�B	�B	�B	�B	!�B	"�B	"�B	#�B	%�B	(�B	.B	1'B	33B	33B	49B	5?B	7LB	:^B	<jB	?}B	A�B	D�B	E�B	F�B	G�B	K�B	N�B	R�B	S�B	T�B	T�B	VB	XB	\)B	^5B	^5B	^5B	_;B	bNB	cTB	dZB	e`B	e`B	e`B	e`B	gmB	hsB	iyB	jB	jB	jB	jB	m�B	o�B	q�B	r�B	t�B	u�B	w�B	x�B	~�B	� B	� B	�B	�B	�B	�B	�%B	�1B	�1B	�7B	�DB	�bB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�-B	�3B	�9B	�?B	�FB	�LB	�XB	�^B	�jB	�jB	�qB	�}B	�}B	�}B	��B	B	B	��B	��B	B	B	B	ÖB	ĜB	ĜB	ÖB	ÖB	ĜB	ŢB	ŢB	ŢB	ĜB	ŢB	ƨB	ǮB	ǮB	ǮB	ȴB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�B	�)B	�/B	�/B	�5B	�5B	�5B	�;B	�BB	�NB	�TB	�ZB	�`B	�`B	�`B	�`B	�`B	�`B	�fB	�fB	�mB	�mB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
+B
1B
+B
+B
+B
1B
1B
1B
1B
1B
	7B
	7B

=B

=B

=B

=B

=B

=B
DB
)B
$Z2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222B	�B	�B	�B	�B	�B	��B	��B
PB
;dB
�oB
�;B
�BB
�HB
�`B
�B
��B
��B%BJB\B\B\B\BbBbBbBbBbB\B\B\BbB{B5?BQ�B[#B_;B^5BcTB>wB/B5?BN�B[#BiyB�B��B�hB�+B� B}�B�=B��B�qB��BŢBǮB��B�#B�B"�B49B>wBA�B2-B'�B#�B"�B5?BQ�BN�BR�BL�BJ�BN�B]/BbNBgmBhsBhsBe`BaHB\)BXBW
BQ�BG�B7LB1'B'�B�B��B�mB�/B��B�dB�B��B�Be`BT�B&�B
�XB
��B
�oB
}�B
XB
=qB
!�B	��B	ȴB	�9B	�B	��B	��B	�=B	p�B	n�B	p�B	n�B	o�B	p�B	iyB	Q�B	A�B	-B	�B	VB	%B	  B�B�HB�5B�#B�BƨB�9B��B��B��B��B��B��B��B��B��B��B�B�3B�?B�FB�FB�RB�dB�FB�B��B��B��B��B��B��B��B�B��B��B��B��B��B��B��B��B�B�B�B�B�B�'B�'B�'B�'B�FB�XB�FB�9B�3B�'B�!B�'B�-B�LB�}B��B�XB�RB�FB�FB�FB�LB�LB�qB�wB�qB��BÖBŢBȴB��B��B��B��B��B��B��B�B�)B�)B�5B�HB�NB�`B�`B�B�B�B�B�B�B�B��B��B��B��B	  B	B	B		7B	PB	hB	oB	uB	�B	�B	�B	�B	�B	�B	�B	!�B	"�B	"�B	#�B	%�B	(�B	.B	1'B	33B	33B	49B	5?B	7LB	:^B	<jB	?}B	A�B	D�B	E�B	F�B	G�B	K�B	N�B	R�B	S�B	T�B	T�B	VB	XB	\)B	^5B	^5B	^5B	_;B	bNB	cTB	dZB	e`B	e`B	e`B	e`B	gmB	hsB	iyB	jB	jB	jB	jB	m�B	o�B	q�B	r�B	t�B	u�B	w�B	x�B	~�B	� B	� B	�B	�B	�B	�B	�%B	�1B	�1B	�7B	�DB	�bB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�-B	�3B	�9B	�?B	�FB	�LB	�XB	�^B	�jB	�jB	�qB	�}B	�}B	�}B	��B	B	B	��B	��B	B	B	B	ÖB	ĜB	ĜB	ÖB	ÖB	ĜB	ŢB	ŢB	ŢB	ĜB	ŢB	ƨB	ǮB	ǮB	ǮB	ȴB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�B	�)B	�/B	�/B	�5B	�5B	�5B	�;B	�BB	�NB	�TB	�ZB	�`B	�`B	�`B	�`B	�`B	�`B	�fB	�fB	�mB	�mB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
+B
1B
+B
+B
+B
1B
1B
1B
1B
1B
	7B
	7B

=B

=B

=B

=B

=B

=B
DB
)B
$Z2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.24 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190549                              AO  ARCAADJP                                                                    20181005190549    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190549  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190549  QCF$                G�O�G�O�G�O�8000            