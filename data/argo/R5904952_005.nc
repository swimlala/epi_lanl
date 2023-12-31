CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:06Z creation      
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
_FillValue                 �  A4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \h   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^\   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �T   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �H   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �<   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �<   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �<   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �<   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �h   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �l   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �p   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �t   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �x   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181005190506  20181005190506  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @ןTĥe�1   @ןUW:Ӭ@3e�Q��c��n��1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      A   A   A   @�  @�  @���AffA@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  BffB   B'��B/��B8  B@  BH  BP  BX  B`  BhffBp  Bx  B��B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B���B�  B�  B�  B�  B�  C   C  C  C  C�fC
  C  C  C�fC  C  C  C�C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<�C>  C@  CA�fCD  CF  CH  CJ  CL  CN  CP  CQ�fCT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv�Cx�Cz  C|  C~  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��C�  C��3C��3C��3C�  C�  C�  C�  C��3C��3C�  C�  C��C�  C��3C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C��3C�  C��C�  C�  C��3C��3C��3C�  C�  C��C�  C��3C�  C��C�  C�  C�  C�  C��3C��3C�  C�  C��C��C�  C�  C�  C��3C�  C��C�  C�  C�  C��3C�  C�  C�  C�  C��3C�  C�  C��3C�  C�  C�  C�  C��3C��3C�  C�  C��3C��3C��3C��3C��3C�  C�  C��3C��3C�  C��3C�  C�  C�  C��C��C�  C��3D   D �fD  Dy�D  D� D  D� D  D� D  D� D  D� D��D� D  D�fD	  D	y�D	��D
� DfD� D  Dy�D  D� D  D� D  D�fD  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��Dy�D��Dy�D   D �fD!fD!� D"fD"� D"��D#� D$  D$y�D%  D%y�D%��D&� D'  D'� D(fD(� D)  D)� D)��D*� D+  D+y�D+��D,�fD,��D-y�D.  D.�fD/  D/� D0  D0� D1  D1y�D2  D2y�D3  D3� D3��D4� D5  D5� D6  D6�fD7  D7� D7��D8y�D8��D9� D:  D:y�D:��D;� D<fD<�fD=fD=� D>  D>� D?  D?� D@  D@� DA  DA� DBfDB� DB��DCy�DD  DD�fDE  DE� DF  DF�fDGfDG� DH  DH�fDH��DI� DJfDJ�fDK  DKy�DL  DL� DM  DM� DM��DN� DO  DOy�DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DXfDX� DX��DY� DZfDZy�D[  D[� D\  D\� D]  D]y�D^  D^�fD_  D_� D`  D`� Da  Day�DbfDb� Dc  Dc� Dd  Dd� De  De�fDe��Df�fDg  Dg� Dh  Dh� Dh��Di� DjfDjy�Dj��Dky�Dk��Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Dsy�Ds��Dty�Dt��Duy�Du��Dvy�Dw  Dw� Dy�
D�0�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��RA\)A@��A`��A�z�A�z�A�z�A�z�A�z�A�z�A�z�A�z�B =qB=qB=qB��B =qB'�B/�B8=qB@=qBH=qBP=qBX=qB`=qBh��Bp=qBx=qB�B��B�Q�B�Q�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C \C\C\C\C��C
\C\C\C��C\C\C\C(�C\C\C\C \C"\C$\C&\C(\C*\C,\C.\C0\C2\C4\C6\C8\C:\C<(�C>\C@\CA��CD\CF\CH\CJ\CL\CN\CP\CQ��CT\CV\CX\CZ\C\\C^\C`\Cb\Cd\Cf\Ch\Cj\Cl\Cn\Cp\Cr\Ct\Cv(�Cx(�Cz\C|\C~\C��C��C��C��C��C���C��C��C��C��C��C��C�{C��C��C��C�{C��C���C���C���C��C��C��C��C���C���C��C��C�{C��C���C���C���C���C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C�{C��C���C���C��C�{C��C��C���C���C���C��C��C�{C��C���C��C�{C��C��C��C��C���C���C��C��C�{C�{C��C��C��C���C��C�{C��C��C��C���C��C��C��C��C���C��C��C���C��C��C��C��C���C���C��C��C���C���C���C���C���C��C��C���C���C��C���C��C��C��C�{C�{C��C���D �D �=D�D}qD�D��D�D��D�D��D�D��D�D��D�qD��D�D�=D	�D	}qD	�qD
��D
=D��D�D}qD�D��D�D��D�D�=D�D��D�D��D�D��D�qD��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�qD}qD�qD}qD �D �=D!
=D!��D"
=D"��D"�qD#��D$�D$}qD%�D%}qD%�qD&��D'�D'��D(
=D(��D)�D)��D)�qD*��D+�D+}qD+�qD,�=D,�qD-}qD.�D.�=D/�D/��D0�D0��D1�D1}qD2�D2}qD3�D3��D3�qD4��D5�D5��D6�D6�=D7�D7��D7�qD8}qD8�qD9��D:�D:}qD:�qD;��D<
=D<�=D=
=D=��D>�D>��D?�D?��D@�D@��DA�DA��DB
=DB��DB�qDC}qDD�DD�=DE�DE��DF�DF�=DG
=DG��DH�DH�=DH�qDI��DJ
=DJ�=DK�DK}qDL�DL��DM�DM��DM�qDN��DO�DO}qDP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX
=DX��DX�qDY��DZ
=DZ}qD[�D[��D\�D\��D]�D]}qD^�D^�=D_�D_��D`�D`��Da�Da}qDb
=Db��Dc�Dc��Dd�Dd��De�De�=De�qDf�=Dg�Dg��Dh�Dh��Dh�qDi��Dj
=Dj}qDj�qDk}qDk�qDl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds}qDs�qDt}qDt�qDu}qDu�qDv}qDw�Dw��Dy��D�2�D��q11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aʰ!AɬAɉ7A�bNA�ZA�Q�A�M�A�C�A�;dA�7LA�5?A�1'A�"�A�{A�  A��A��mA��HA��#A��;A���A���A��A��yA��yA���AȶFAȕ�Aț�Aȡ�Aȣ�Aȇ+A�p�A�r�A�p�Aȇ+AȋDA���A��mA�|�A�ZA���A�ĜA�ƨA��
Aǧ�A�hsA��A���AƼjAƗ�AŲ-A��AļjA���A��A��A�x�A��wA��mA��A�33A��A���A��A�1'A�`BA��\A���A�%A���A�5?A��A�A�x�A�S�A�oA�z�A��A���A�t�A�\)A�(�A��TA�1A�|�A�9XA��A�\)A�+A���A��A��`A��RA�5?A�p�A�33A��jA��/A�E�A��hA�=qA��+A��
A��A�;dA��A��A��jA�%A��7A���A���A��7A�O�A~�uA~(�A~{A}�FAyAt��Ap�RAm�Al=qAh�AfbAc+A]�AVZAP��AM��AK�-AIXAHĜAF��AC��AA|�A@1'A>�/A=A<r�A;t�A:�+A8E�A6 �A5A4�RA3+A1�A/"�A.(�A-�;A-��A-t�A,�HA+�#A*�HA)%A$�A#|�A"�DA ^5A&�A�jAffA�TA�PAXAr�A?}AE�AȴA�FA/A�!AoA��A�\A �A�A�DA��A��A��Ax�A�uA\)A"�A
1A��A�FA�RA1'A-A-A�jA~�A�^A�A�PA ��@��^@���@���@��@�|�@�x�@�^@���@��@�Ĝ@�r�@�ƨ@�+@�u@�5?@�&�@�(�@㕁@�33@�J@��@ߕ�@�-@�?}@ܴ9@��@�33@�5?@ٙ�@�7L@؋D@׾w@��#@�C�@���@�ff@Ѳ-@�/@У�@�Q�@�  @϶F@���@�%@�b@˝�@�l�@�\)@�\)@�S�@�C�@��@��T@���@ț�@��;@���@Ɨ�@�V@�E�@�M�@�J@š�@š�@ř�@ř�@�`B@�V@ļj@Ĭ@ě�@�z�@Ý�@�t�@�t�@�|�@���@�$�@�&�@���@��@���@��@�{@��#@�G�@���@���@��@��`@��j@��D@�1@��@�|�@�K�@�@���@�^5@�-@�@�@�p�@�7L@��@��;@��R@��@�O�@��`@��9@��D@�  @��P@�S�@�+@��@�n�@�hs@��/@��@��@���@�Z@���@�ƨ@���@��w@��
@��F@�dZ@�@�-@�E�@�{@�{@�$�@�n�@�ȴ@�dZ@���@�ƨ@���@��@�  @��w@��P@�C�@�C�@��y@�n�@��#@��h@�x�@�/@��@��@�j@�1'@�b@���@�dZ@��@�ȴ@�^5@�J@��#@�X@��9@�1@���@�ƨ@�;d@���@�$�@�@��@�hs@��@��9@��u@�r�@�1'@�b@���@��;@�l�@�+@��@�o@��@�ȴ@���@���@���@��R@���@���@�=q@���@�O�@��@�%@��`@�z�@�b@��;@��
@���@���@�|�@���@��\@���@���@�n�@���@�X@���@�A�@��@�|�@�ȴ@�n�@��#@��@��@�l�@��y@��R@�~�@�=q@�J@���@��^@��^@���@�`B@�%@�  @�l�@��@���@�ȴ@��\@�V@��T@���@��7@�X@��@��@��j@��@�I�@�1@�t�@�C�@�"�@��@��@��R@��!@���@�v�@�M�@�@��T@��T@���@��^@���@��@�?}@��@��9@� �@���@���@���@�l�@�+@��@��H@��@���@��!@��@���@�hs@�X@�X@�G�@��@� @pM11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Aʰ!AɬAɉ7A�bNA�ZA�Q�A�M�A�C�A�;dA�7LA�5?A�1'A�"�A�{A�  A��A��mA��HA��#A��;A���A���A��A��yA��yA���AȶFAȕ�Aț�Aȡ�Aȣ�Aȇ+A�p�A�r�A�p�Aȇ+AȋDA���A��mA�|�A�ZA���A�ĜA�ƨA��
Aǧ�A�hsA��A���AƼjAƗ�AŲ-A��AļjA���A��A��A�x�A��wA��mA��A�33A��A���A��A�1'A�`BA��\A���A�%A���A�5?A��A�A�x�A�S�A�oA�z�A��A���A�t�A�\)A�(�A��TA�1A�|�A�9XA��A�\)A�+A���A��A��`A��RA�5?A�p�A�33A��jA��/A�E�A��hA�=qA��+A��
A��A�;dA��A��A��jA�%A��7A���A���A��7A�O�A~�uA~(�A~{A}�FAyAt��Ap�RAm�Al=qAh�AfbAc+A]�AVZAP��AM��AK�-AIXAHĜAF��AC��AA|�A@1'A>�/A=A<r�A;t�A:�+A8E�A6 �A5A4�RA3+A1�A/"�A.(�A-�;A-��A-t�A,�HA+�#A*�HA)%A$�A#|�A"�DA ^5A&�A�jAffA�TA�PAXAr�A?}AE�AȴA�FA/A�!AoA��A�\A �A�A�DA��A��A��Ax�A�uA\)A"�A
1A��A�FA�RA1'A-A-A�jA~�A�^A�A�PA ��@��^@���@���@��@�|�@�x�@�^@���@��@�Ĝ@�r�@�ƨ@�+@�u@�5?@�&�@�(�@㕁@�33@�J@��@ߕ�@�-@�?}@ܴ9@��@�33@�5?@ٙ�@�7L@؋D@׾w@��#@�C�@���@�ff@Ѳ-@�/@У�@�Q�@�  @϶F@���@�%@�b@˝�@�l�@�\)@�\)@�S�@�C�@��@��T@���@ț�@��;@���@Ɨ�@�V@�E�@�M�@�J@š�@š�@ř�@ř�@�`B@�V@ļj@Ĭ@ě�@�z�@Ý�@�t�@�t�@�|�@���@�$�@�&�@���@��@���@��@�{@��#@�G�@���@���@��@��`@��j@��D@�1@��@�|�@�K�@�@���@�^5@�-@�@�@�p�@�7L@��@��;@��R@��@�O�@��`@��9@��D@�  @��P@�S�@�+@��@�n�@�hs@��/@��@��@���@�Z@���@�ƨ@���@��w@��
@��F@�dZ@�@�-@�E�@�{@�{@�$�@�n�@�ȴ@�dZ@���@�ƨ@���@��@�  @��w@��P@�C�@�C�@��y@�n�@��#@��h@�x�@�/@��@��@�j@�1'@�b@���@�dZ@��@�ȴ@�^5@�J@��#@�X@��9@�1@���@�ƨ@�;d@���@�$�@�@��@�hs@��@��9@��u@�r�@�1'@�b@���@��;@�l�@�+@��@�o@��@�ȴ@���@���@���@��R@���@���@�=q@���@�O�@��@�%@��`@�z�@�b@��;@��
@���@���@�|�@���@��\@���@���@�n�@���@�X@���@�A�@��@�|�@�ȴ@�n�@��#@��@��@�l�@��y@��R@�~�@�=q@�J@���@��^@��^@���@�`B@�%@�  @�l�@��@���@�ȴ@��\@�V@��T@���@��7@�X@��@��@��j@��@�I�@�1@�t�@�C�@�"�@��@��@��R@��!@���@�v�@�M�@�@��T@��T@���@��^@���@��@�?}@��@��9@� �@���@���@���@�l�@�+@��@��H@��@���@��!@��@���@�hs@�X@�X@�G�@��@� @pM11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�5B	�BB	�ZB
�B
��B
��B
��B
��B
��B
�B
�HB
�fB
�B
��B
��B�B;dB=qBP�BXB]/BgmB�B�7B�oB��B�'B�RBɺB�/B�yB�B#�B2-BE�B_;Bo�Bu�Bx�By�By�Bx�Bw�Bx�By�B|�B}�B� B�B�\B�VB�oB�bB�PB�bB��B��B��B��B�bB�1Bs�B`BBYBT�BO�BG�B.BbBJBJB+B��B�B��BB�-B��Bx�BaHBJ�BA�B=qB7LB0!B�BB
�B
�
B
ǮB
��B
q�B
D�B
7LB
6FB
5?B
1'B
{B	�B	��B	�RB	�B	��B	�+B	p�B	E�B	\B�B�ZB�`B�mB�NB�
B��BȴBÖBÖBƨBȴB��B��B��B��B��B��B��BǮBƨBǮBǮBǮBǮBɺBȴBŢB��B�9B�B��B�hB�+B�B�B|�Bz�By�B|�B�B�B� B�B�B�Bv�Bq�Bp�Bp�Br�Bx�B{�Bz�Bv�Bs�Bt�B�B�%B�Bz�B{�Bz�B}�B~�B�B�PB��B��B��B�VB�7B�B{�B|�B� B� B~�Bx�Bv�Bv�Bu�Bs�Bp�Bm�Bm�Bp�Bq�Bs�Bt�Bt�Bv�Bw�Bw�By�Bx�By�By�By�B{�B{�B{�B{�B{�B~�B~�B�B�+B�1B�7B�7B�=B�DB�JB�\B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�?B�RB�}BBÖBŢBƨBǮB��B��B��B�B�5B�NB�B�B�B��B��B��B��B	B	B	B	+B		7B	
=B	
=B	
=B	JB	PB	\B	bB	hB	oB	uB	�B	�B	�B	�B	�B	�B	�B	�B	$�B	+B	.B	0!B	49B	5?B	5?B	7LB	:^B	;dB	;dB	>wB	B�B	K�B	N�B	O�B	O�B	O�B	R�B	T�B	YB	dZB	e`B	gmB	o�B	r�B	v�B	x�B	{�B	}�B	� B	�B	�B	�7B	�hB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�-B	�?B	�^B	�jB	�jB	�qB	�}B	��B	��B	B	ÖB	ĜB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�)B	�)B	�/B	�5B	�5B	�5B	�5B	�;B	�;B	�;B	�;B	�BB	�HB	�NB	�NB	�NB	�NB	�NB	�NB	�NB	�TB	�ZB	�`B	�`B	�`B	�fB	�sB	�sB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
%B
%B
%B
+B
1B
1B
1B
1B
1B
	7B
	7B
	7B

=B

=B

=B
�B
~B
(�22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�5B	�BB	�ZB
�B
��B
��B
��B
��B
��B
�B
�HB
�fB
�B
��B
��B�B;dB=qBP�BXB]/BgmB�B�7B�oB��B�'B�RBɺB�/B�yB�B#�B2-BE�B_;Bo�Bu�Bx�By�By�Bx�Bw�Bx�By�B|�B}�B� B�B�\B�VB�oB�bB�PB�bB��B��B��B��B�bB�1Bs�B`BBYBT�BO�BG�B.BbBJBJB+B��B�B��BB�-B��Bx�BaHBJ�BA�B=qB7LB0!B�BB
�B
�
B
ǮB
��B
q�B
D�B
7LB
6FB
5?B
1'B
{B	�B	��B	�RB	�B	��B	�+B	p�B	E�B	\B�B�ZB�`B�mB�NB�
B��BȴBÖBÖBƨBȴB��B��B��B��B��B��B��BǮBƨBǮBǮBǮBǮBɺBȴBŢB��B�9B�B��B�hB�+B�B�B|�Bz�By�B|�B�B�B� B�B�B�Bv�Bq�Bp�Bp�Br�Bx�B{�Bz�Bv�Bs�Bt�B�B�%B�Bz�B{�Bz�B}�B~�B�B�PB��B��B��B�VB�7B�B{�B|�B� B� B~�Bx�Bv�Bv�Bu�Bs�Bp�Bm�Bm�Bp�Bq�Bs�Bt�Bt�Bv�Bw�Bw�By�Bx�By�By�By�B{�B{�B{�B{�B{�B~�B~�B�B�+B�1B�7B�7B�=B�DB�JB�\B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�?B�RB�}BBÖBŢBƨBǮB��B��B��B�B�5B�NB�B�B�B��B��B��B��B	B	B	B	+B		7B	
=B	
=B	
=B	JB	PB	\B	bB	hB	oB	uB	�B	�B	�B	�B	�B	�B	�B	�B	$�B	+B	.B	0!B	49B	5?B	5?B	7LB	:^B	;dB	;dB	>wB	B�B	K�B	N�B	O�B	O�B	O�B	R�B	T�B	YB	dZB	e`B	gmB	o�B	r�B	v�B	x�B	{�B	}�B	� B	�B	�B	�7B	�hB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�-B	�?B	�^B	�jB	�jB	�qB	�}B	��B	��B	B	ÖB	ĜB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�)B	�)B	�/B	�5B	�5B	�5B	�5B	�;B	�;B	�;B	�;B	�BB	�HB	�NB	�NB	�NB	�NB	�NB	�NB	�NB	�TB	�ZB	�`B	�`B	�`B	�fB	�sB	�sB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
%B
%B
%B
+B
1B
1B
1B
1B
1B
	7B
	7B
	7B

=B

=B

=B
�B
~B
(�22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.06 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190506                              AO  ARCAADJP                                                                    20181005190506    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190506  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190506  QCF$                G�O�G�O�G�O�8000            