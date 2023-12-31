CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:16:56Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181005191656  20181005191656  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               "A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @׷����1   @׷���@5&�x���c؃n��1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      "A   A   A   @�ff@�  A   A   A@  A`  A�  A�33A�  A�  A���A���A�33A�  B   B  B  B  B   B(  B0  B8  B@  BHffBP  BX  B`  Bh  Bp  Bx  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  C   C�C  C  C  C
  C  C  C  C�fC  C  C  C  C  C  C   C!�fC$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CS�fCV  CX  CZ  C\�C^�C`  Cb  Cd  Cf  Cg�fCj  Cl�Cn  Co�fCr  Ct  Cv  Cx  Cz  C|  C~  C�fC��3C�  C��C��C�  C�  C�  C��C�  C�  C�  C��3C�  C��C��3C��3C��fC��3C��3C��3C�  C��C��C��C�  C�  C��3C�  C�  C�  C��3C�  C�  C�  C��C�  C�  C�  C��C��3C��C��C��C��C�  C��fC��3C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C��3C��3C�  C��3C�  C�  C�  C��C��C�  C��3C��C��C��3C��3C��3C��3C��3C��fC��3C��3C��fC��3C��3C��3C�  C�  C�  C��C��C��C�  C��3C�  C��C�  C�  C��C��3C��C�  C��3C�  C��C�  C�  C�  C��3C��3C��3C�  C�  C��3C�  C��3C�  C�  C�  C��3C�  C��C��C�  C��3C�  C��C��C�  C�  C��3C��3D � D ��Dy�D  D�fD  D� D  D� DfD�fDfD� D  D� D��Dy�D	  D	�fD
fD
�fDfD�fDfD� DfD� D��D� D  Dy�D��D� D  D� D  D� D  Dy�D  D�fDfD�fD  Dy�D��D� D  D� DfD�fDfD� D  D� D��Ds3D��Dy�D  D�fD  Dy�D   D �fD ��D!� D"  D"y�D"��D#y�D#��D$� D%  D%�fD&�D&�fD'  D'� D'��D(� D)  D)� D)��D*y�D+  D+�fD,fD,� D,��D-y�D-��D.� D/  D/y�D0fD0�fD1fD1�fD1��D2y�D2��D3� D4  D4� D5  D5�fD6fD6� D7  D7y�D7��D8y�D9  D9� D:fD:� D:��D;� D<  D<�fD=  D=� D>  D>y�D>��D?� D@  D@� DA  DA� DBfDB�fDB��DCy�DD  DD� DE  DEy�DF  DF� DGfDG� DG��DH� DI  DI�fDJfDJ�fDK  DK� DL  DL� DM  DM�fDN�DN�fDO  DO� DP  DP�fDQ  DQ� DR�DR�fDSfDS��DTfDT� DU  DU� DU��DVy�DV��DW� DX  DXy�DY  DY�fDZ  DZy�D[fD[�fD\  D\y�D]  D]� D^  D^� D_  D_� D_��D`y�Da  Da� Db  Db�fDc  Dc� Dd  Dd� DefDe�fDf  Dfy�Dg  Dg� Dh  Dh�fDifDi� Dj  Dj�fDj��Dk� DlfDl�fDmfDm� Dm��Dny�Do  Do� Dp  Dp� Dq  Dq�fDrfDr�fDs  Dsy�Dt  Dt� DufDu�fDv  Dv� DwfDwffDy��D�S�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�z@ǮA�
A#�
AC�
Ac�
A��A��A��A��A¸RAҸRA��A��B ��B��B��B��B ��B(��B0��B8��B@��BI\)BP��BX��B`��Bh��Bp��Bx��B�z�B��B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B��B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�G�B�z�B�z�B�z�B�z�B�z�C =qCWC=qC=qC=qC
=qC=qC=qC=qC#�C=qC=qC=qC=qC=qC=qC =qC"#�C$=qC&=qC(=qC*=qC,=qC.=qC0=qC2=qC4=qC6=qC8=qC:=qC<=qC>=qC@=qCB=qCD=qCF=qCH=qCJ=qCL=qCN=qCP=qCR=qCT#�CV=qCX=qCZ=qC\WC^WC`=qCb=qCd=qCf=qCh#�Cj=qClWCn=qCp#�Cr=qCt=qCv=qCx=qCz=qC|=qC~=qC��C��C��C�+�C�+�C��C��C��C�+�C��C��C��C��C��C�+�C��C��C�C��C��C��C��C�+�C�+�C�+�C��C��C��C��C��C��C��C��C��C��C�+�C��C��C��C�+�C��C�+�C�+�C�+�C�+�C��C�C��C��C��C��C��C�+�C��C��C��C��C��C��C��C��C��C��C��C��C�+�C�+�C��C��C�+�C�+�C��C��C��C��C��C�C��C��C�C��C��C��C��C��C��C�+�C�+�C�+�C��C��C��C�+�C��C��C�+�C��C�+�C��C��C��C�+�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�+�C�+�C��C��C��C�+�C�+�C��C��C��D �D �\D�D��D\D��D\D�\D\D�\D�D��D�D�\D\D�\D�D��D	\D	��D
�D
��D�D��D�D�\D�D�\D�D�\D\D��D�D�\D\D�\D\D�\D\D��D\D��D�D��D\D��D�D�\D\D�\D�D��D�D�\D\D�\D�D��D�D��D\D��D\D��D \D ��D!�D!�\D"\D"��D#�D#��D$�D$�\D%\D%��D&)D&��D'\D'�\D(�D(�\D)\D)�\D*�D*��D+\D+��D,�D,�\D-�D-��D.�D.�\D/\D/��D0�D0��D1�D1��D2�D2��D3�D3�\D4\D4�\D5\D5��D6�D6�\D7\D7��D8�D8��D9\D9�\D:�D:�\D;�D;�\D<\D<��D=\D=�\D>\D>��D?�D?�\D@\D@�\DA\DA�\DB�DB��DC�DC��DD\DD�\DE\DE��DF\DF�\DG�DG�\DH�DH�\DI\DI��DJ�DJ��DK\DK�\DL\DL�\DM\DM��DN)DN��DO\DO�\DP\DP��DQ\DQ�\DR)DR��DS�DS�)DT�DT�\DU\DU�\DV�DV��DW�DW�\DX\DX��DY\DY��DZ\DZ��D[�D[��D\\D\��D]\D]�\D^\D^�\D_\D_�\D`�D`��Da\Da�\Db\Db��Dc\Dc�\Dd\Dd�\De�De��Df\Df��Dg\Dg�\Dh\Dh��Di�Di�\Dj\Dj��Dk�Dk�\Dl�Dl��Dm�Dm�\Dn�Dn��Do\Do�\Dp\Dp�\Dq\Dq��Dr�Dr��Ds\Ds��Dt\Dt�\Du�Du��Dv\Dv�\Dw�Dwu�Dy�RD�[31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AՕ�AՕ�AՕ�AՓuAՓuAՕ�A՗�A՛�Aՙ�AՑhAՁAԋDA�  AЗ�A�(�A̾wA�t�A�Q�A��A���A�A�A�ffA���Aŏ\A�S�A��#AĲ-A�A�p�A�A��A�A\A�7LA��mA�C�A��TA�z�A���A��RA�z�A�I�A��HA��uA�VA�oA���A��A��/A��A�G�A�A�C�A��PA�z�A�%A�XA�~�A��DA�I�A��A�~�A�/A��A��`A�oA�l�A�bA�ĜA�`BA��/A�VA��yA�r�A���A��A�p�A�A�A���A��9A�x�A��A�9XA��9A�VA�I�A��A��A�+A�p�A�^5A�t�A���A���A���A�Q�A�/A��!A��9A��A��A���A���A��7A�/A�VA�1'A��A�1'A���A�ĜA���A�O�A}�#Ay��Avv�At��As��Aq��Am��Am
=Akl�Ai�FAh{AfĜAe�hAaO�A^9XA\��AYƨAX �AW��AV�9AU��AS��AP�AN�jAM;dAL5?AJ�RAG%A@9XA<1A;"�A:$�A9O�A9�A8bNA7�A6ZA3�7A2jA2E�A2-A1��A1x�A0�A.�uA-|�A,��A,bNA+��A*�9A)�^A)A(n�A'�FA&5?A$�/A#oA �\AK�A?}AVA��A�!AZA�^Ar�A�A�A{AJA;dA��A�A-AE�A�#A�yA$�AK�A$�A9XA��A�A
I�A	�A	x�A	+AbNA�7A�uAJAA�A�-A?}AȴA��AC�A�\A�;A��A;dA �\@���@��y@�@��@�o@�@�r�@��/@�@�@�/@��/@��/@�1'@�+@�M�@�?}@��@�@�M�@�j@��T@��/@�j@�I�@�F@��@�h@�  @���@ݡ�@�A�@��@�?}@���@�@�bN@�-@д9@� �@�ƨ@�o@Χ�@ͺ^@��@�S�@�o@���@�v�@ȼj@�K�@�&�@���@�r�@�1'@�Z@�r�@�z�@�1'@���@�
=@�n�@�ff@�E�@���@���@���@�|�@�
=@��H@��!@�ff@���@�hs@���@�I�@�  @�dZ@��T@�?}@��j@�I�@� �@���@��;@��@��@�|�@�
=@���@�~�@�V@�-@�{@��#@�x�@��`@�z�@�(�@��@�1@�  @��@��@��m@��
@���@���@��F@�l�@�K�@�+@��@���@��!@���@�ff@���@��^@��@�hs@�O�@�@��@�J@�x�@�7L@�7L@���@��@��/@���@�r�@�33@�-@��j@�t�@�"�@���@�~�@�^5@�5?@�@���@�G�@�9X@��;@�ƨ@���@�dZ@�"�@���@�-@�{@�@�@��@�&�@�V@�%@���@��/@���@�Ĝ@��j@��j@��u@�j@�1'@�ƨ@�|�@��@��!@�J@���@� �@�t�@�@���@���@�^5@�E�@���@��h@��7@�x�@�p�@���@��j@�9X@��w@��P@�|�@�|�@�K�@�
=@�@���@��@�ȴ@���@���@��+@�n�@�M�@�J@��^@�?}@���@�r�@�1'@�1@��@�dZ@��!@�~�@�v�@�n�@�ff@�^5@�M�@���@�x�@�X@�7L@�V@��@��9@��j@��9@���@�j@�A�@��@��F@��@�t�@��H@�E�@��@���@���@���@��9@�j@�(�@��m@���@�ƨ@��@�|�@�;d@�o@��@�ȴ@��\@�^5@�ff@�-@��@���@��@�V@�(�@��
@�l�@��@���@�^5@�-@�{@���@�p�@��/@�Q�@�1@��@��m@�
=@~Z�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111AՕ�AՕ�AՕ�AՓuAՓuAՕ�A՗�A՛�Aՙ�AՑhAՁAԋDA�  AЗ�A�(�A̾wA�t�A�Q�A��A���A�A�A�ffA���Aŏ\A�S�A��#AĲ-A�A�p�A�A��A�A\A�7LA��mA�C�A��TA�z�A���A��RA�z�A�I�A��HA��uA�VA�oA���A��A��/A��A�G�A�A�C�A��PA�z�A�%A�XA�~�A��DA�I�A��A�~�A�/A��A��`A�oA�l�A�bA�ĜA�`BA��/A�VA��yA�r�A���A��A�p�A�A�A���A��9A�x�A��A�9XA��9A�VA�I�A��A��A�+A�p�A�^5A�t�A���A���A���A�Q�A�/A��!A��9A��A��A���A���A��7A�/A�VA�1'A��A�1'A���A�ĜA���A�O�A}�#Ay��Avv�At��As��Aq��Am��Am
=Akl�Ai�FAh{AfĜAe�hAaO�A^9XA\��AYƨAX �AW��AV�9AU��AS��AP�AN�jAM;dAL5?AJ�RAG%A@9XA<1A;"�A:$�A9O�A9�A8bNA7�A6ZA3�7A2jA2E�A2-A1��A1x�A0�A.�uA-|�A,��A,bNA+��A*�9A)�^A)A(n�A'�FA&5?A$�/A#oA �\AK�A?}AVA��A�!AZA�^Ar�A�A�A{AJA;dA��A�A-AE�A�#A�yA$�AK�A$�A9XA��A�A
I�A	�A	x�A	+AbNA�7A�uAJAA�A�-A?}AȴA��AC�A�\A�;A��A;dA �\@���@��y@�@��@�o@�@�r�@��/@�@�@�/@��/@��/@�1'@�+@�M�@�?}@��@�@�M�@�j@��T@��/@�j@�I�@�F@��@�h@�  @���@ݡ�@�A�@��@�?}@���@�@�bN@�-@д9@� �@�ƨ@�o@Χ�@ͺ^@��@�S�@�o@���@�v�@ȼj@�K�@�&�@���@�r�@�1'@�Z@�r�@�z�@�1'@���@�
=@�n�@�ff@�E�@���@���@���@�|�@�
=@��H@��!@�ff@���@�hs@���@�I�@�  @�dZ@��T@�?}@��j@�I�@� �@���@��;@��@��@�|�@�
=@���@�~�@�V@�-@�{@��#@�x�@��`@�z�@�(�@��@�1@�  @��@��@��m@��
@���@���@��F@�l�@�K�@�+@��@���@��!@���@�ff@���@��^@��@�hs@�O�@�@��@�J@�x�@�7L@�7L@���@��@��/@���@�r�@�33@�-@��j@�t�@�"�@���@�~�@�^5@�5?@�@���@�G�@�9X@��;@�ƨ@���@�dZ@�"�@���@�-@�{@�@�@��@�&�@�V@�%@���@��/@���@�Ĝ@��j@��j@��u@�j@�1'@�ƨ@�|�@��@��!@�J@���@� �@�t�@�@���@���@�^5@�E�@���@��h@��7@�x�@�p�@���@��j@�9X@��w@��P@�|�@�|�@�K�@�
=@�@���@��@�ȴ@���@���@��+@�n�@�M�@�J@��^@�?}@���@�r�@�1'@�1@��@�dZ@��!@�~�@�v�@�n�@�ff@�^5@�M�@���@�x�@�X@�7L@�V@��@��9@��j@��9@���@�j@�A�@��@��F@��@�t�@��H@�E�@��@���@���@���@��9@�j@�(�@��m@���@�ƨ@��@�|�@�;d@�o@��@�ȴ@��\@�^5@�ff@�-@��@���@��@�V@�(�@��
@�l�@��@���@�^5@�-@�{@���@�p�@��/@�Q�@�1@��@��m@�
=@~Z�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B\B\B\BbBbBbBbB\BbBbBbB�B1'BN�B�B�'B�-B�jB��B�B�5B�`B�B�#B�B�ZB�B�B>wBK�BL�BP�BYBdZBk�Bx�B~�B�%B�uB��B��B��B��B��B��B�B�B�B�B�!B�-B�XB��BǮBǮB��B��B�/B�sB��B��B��B  BBBBBB  B��B��B�B�yB�NB�B�9B�PBl�BM�B;dB#�B�BDB�B�`B��B��B�5B��BÖB�Bx�BD�B49B/B$�B
�B
�B
ƨB
�3B
��B
�B
~�B
�%B
��B
��B
��B
��B
��B
�bB
�B
|�B
r�B
]/B
?}B
&�B
�B
\B
B	�B	�ZB	�B	��B	B	�LB	�B	��B	�B	v�B	hsB	]/B	YB	S�B	L�B	B�B	1'B	%�B	�B	�B	DB��B�BǮBĜBB�}B�}B�jB�RB�9B�'B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B��B�{B�uB�oB�hB�hB�bB�bB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�uB�oB�oB�hB�hB�bB�\B�\B�\B�hB�bB�bB�\B�hB�\B�\B�bB�hB�\B�JB�bB�{B��B��B��B��B��B��B��B��B��B��B�B�-B�FB�RB�dB�qB�}BÖBĜBƨB��B��B��B��B�B�
B�B�)B�BB�HB�BB�BB�ZB�fB�`B�`B�fB�B�B�B�B�B�B�B��B��B��B��B��B	  B	B	DB	PB	VB	VB	PB	VB	oB	�B	�B	�B	%�B	(�B	+B	-B	-B	.B	.B	/B	0!B	0!B	49B	5?B	6FB	7LB	7LB	7LB	7LB	8RB	:^B	;dB	<jB	<jB	<jB	=qB	>wB	>wB	>wB	?}B	?}B	@�B	A�B	B�B	C�B	C�B	E�B	F�B	G�B	G�B	I�B	L�B	N�B	R�B	T�B	YB	_;B	e`B	hsB	ffB	ffB	gmB	gmB	jB	s�B	y�B	z�B	{�B	z�B	z�B	|�B	}�B	�B	�B	�B	�B	�B	�B	�%B	�7B	�=B	�DB	�JB	�\B	�bB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�!B	�3B	�9B	�-B	�!B	�!B	�-B	�?B	�FB	�LB	�XB	�XB	�jB	�wB	�}B	��B	ÖB	ĜB	ŢB	ǮB	ɺB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�
B	�B	�)B	�/B	�/B	�/B	�/B	�/B	�/B	�5B	�;B	�;B	�BB	�HB	�NB	�TB	�TB	�TB	�TB	�`B	�`B	�fB	�mB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
%B
%B
+B
�B
�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222B\B\B\BbBbBbBbB\BbBbBbB�B1'BN�B�B�'B�-B�jB��B�B�5B�`B�B�#B�B�ZB�B�B>wBK�BL�BP�BYBdZBk�Bx�B~�B�%B�uB��B��B��B��B��B��B�B�B�B�B�!B�-B�XB��BǮBǮB��B��B�/B�sB��B��B��B  BBBBBB  B��B��B�B�yB�NB�B�9B�PBl�BM�B;dB#�B�BDB�B�`B��B��B�5B��BÖB�Bx�BD�B49B/B$�B
�B
�B
ƨB
�3B
��B
�B
~�B
�%B
��B
��B
��B
��B
��B
�bB
�B
|�B
r�B
]/B
?}B
&�B
�B
\B
B	�B	�ZB	�B	��B	B	�LB	�B	��B	�B	v�B	hsB	]/B	YB	S�B	L�B	B�B	1'B	%�B	�B	�B	DB��B�BǮBĜBB�}B�}B�jB�RB�9B�'B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B��B�{B�uB�oB�hB�hB�bB�bB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�uB�oB�oB�hB�hB�bB�\B�\B�\B�hB�bB�bB�\B�hB�\B�\B�bB�hB�\B�JB�bB�{B��B��B��B��B��B��B��B��B��B��B�B�-B�FB�RB�dB�qB�}BÖBĜBƨB��B��B��B��B�B�
B�B�)B�BB�HB�BB�BB�ZB�fB�`B�`B�fB�B�B�B�B�B�B�B��B��B��B��B��B	  B	B	DB	PB	VB	VB	PB	VB	oB	�B	�B	�B	%�B	(�B	+B	-B	-B	.B	.B	/B	0!B	0!B	49B	5?B	6FB	7LB	7LB	7LB	7LB	8RB	:^B	;dB	<jB	<jB	<jB	=qB	>wB	>wB	>wB	?}B	?}B	@�B	A�B	B�B	C�B	C�B	E�B	F�B	G�B	G�B	I�B	L�B	N�B	R�B	T�B	YB	_;B	e`B	hsB	ffB	ffB	gmB	gmB	jB	s�B	y�B	z�B	{�B	z�B	z�B	|�B	}�B	�B	�B	�B	�B	�B	�B	�%B	�7B	�=B	�DB	�JB	�\B	�bB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�!B	�3B	�9B	�-B	�!B	�!B	�-B	�?B	�FB	�LB	�XB	�XB	�jB	�wB	�}B	��B	ÖB	ĜB	ŢB	ǮB	ɺB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�
B	�B	�)B	�/B	�/B	�/B	�/B	�/B	�/B	�5B	�;B	�;B	�BB	�HB	�NB	�TB	�TB	�TB	�TB	�`B	�`B	�fB	�mB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
%B
%B
+B
�B
�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.24 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191656                              AO  ARCAADJP                                                                    20181005191656    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191656  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191656  QCF$                G�O�G�O�G�O�8000            