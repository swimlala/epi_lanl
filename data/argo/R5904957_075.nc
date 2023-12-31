CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:18Z creation      
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
_FillValue                    �@Argo profile    3.1 1.2 19500101000000  20181024140818  20181024140818  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               KA   AO  6560                            2B  A   APEX                            7471                            062512                          846 @��e�&d1   @��e��@2`     �c�bM��1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      KA   A   B   @���@�  @���A   A@  A`  A�  A�  A�  A�  A�  A�  A���A�  B   B  B  B  B ffB'��B0  B8  B?��BH  BP  BX  B`  Bh  Bp  Bx  B�33B�33B�33B���B�  B�  B�  B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C  C  C   C"�C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CG�fCJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd�Cf  Ch  Ci�fCk�fCn  Cp  Cr  Ct  Cv�Cx�Cz  C|  C~  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��C�  C�  D   D � D  D� D  D� D  D� DfD�fD  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  Dy�D��Dy�D  D� D   D � D!  D!� D"  D"� D#  D#�fD$fD$�fD%  D%� D&fD&�fD'fD'� D(  D(� D)  D)� D*  D*y�D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5y�D6  D6� D7  D7� D8  D8� D9fD9� D:  D:� D;  D;� D<  D<� D<��D=� D>  D>�fD?  D?� D@  D@� DA  DA� DBfDB� DC  DC� DDfDD� DE  DE� DF  DF� DGfDG� DH  DH� DI  DI� DJfDJ�fDK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT�fDU  DUy�DV  DV� DW  DW� DX  DXy�DY  DY�fDZ  DZy�D[  D[� D\fD\�fD]  D]� D^  D^� D_  D_�fD`  D`� Da  Da� Db  Db� Dc  Dc� DdfDd�fDe  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk�fDlfDl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� DqfDq� Dr  Dr�fDs  Ds� DtfDt� Du  Du� Dv  Dv� Dw  Dw� Dw�3Dy� D�@ D�ff1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @���@�\)A{A#�AC�Ac�A��
A��
A��
A��
A��
A��
A��A��
B �B�B�B�B!Q�B(�B0�B8�B@�BH�BP�BX�B`�Bh�Bp�Bx�B���B���B���B�]B�u�B�u�B�u�B���B�B�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�C:�C:�C :�C"T{C$:�C&:�C(:�C*:�C,:�C.:�C0:�C2:�C4:�C6:�C8:�C::�C<:�C>:�C@:�CB:�CD:�CF:�CH!GCJ:�CL:�CN:�CP:�CR:�CT:�CV:�CX:�CZ:�C\:�C^:�C`:�Cb:�CdT{Cf:�Ch:�Cj!GCl!GCn:�Cp:�Cr:�Ct:�CvT{CxT{Cz:�C|:�C~:�C�qC�qC�qC�qC�qC�*>C�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC��C��C��C�qC�qC�qC��C�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC��C�qC�qC�qC�qC�qC�qC�qC��C��C�qC�qC�qC�qC�qC�*>C�qC�qC�qC�qC�*>C�qC��C�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�*>C�*>C�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�*>C�qC�qC�qC�qC�qC�*>C�qC�qD �D ��D�D��D�D��D�D��DD�D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D�RD�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D�RD�D��D�D�RDRD�RD�D��D �D ��D!�D!��D"�D"��D#�D#�D$D$�D%�D%��D&D&�D'D'��D(�D(��D)�D)��D*�D*�RD+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5�RD6�D6��D7�D7��D8�D8��D9D9��D:�D:��D;�D;��D<�D<��D=RD=��D>�D>�D?�D?��D@�D@��DA�DA��DBDB��DC�DC��DDDD��DE�DE��DF�DF��DGDG��DH�DH��DI�DI��DJDJ�DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT�DU�DU�RDV�DV��DW�DW��DX�DX�RDY�DY�DZ�DZ�RD[�D[��D\D\�D]�D]��D^�D^��D_�D_�D`�D`��Da�Da��Db�Db��Dc�Dc��DdDd�De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk�DlDl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��DqDq��Dr�Dr�Ds�Ds��DtDt��Du�Du��Dv�Dv��Dw�Dw��Dw��Dy��D�G\D�m�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�~�A�~�A�z�A߃A߉7AߋDA߉7A߉7AߋDAߋDAߋDAߍPAߏ\AߋDA߃A�r�A�K�A�-A��mA޲-A�bNA�A�S�A٬A���AؑhA�n�A�33A��A�bA���A�(�A�%A�v�A��mA�1'A�ȴA�A�A��A�ȴA�ƨA���A�G�A�n�A�A�A��\A���A�|�A��TA��A���A�A�^5A�z�A�x�A� �A��A�^5A���A�~�A��A���A�A���A�M�A���A��`A��A�t�A��-A��-A��A��#A�l�A�jA���A�ĜA��A�S�A~��A}�Awx�At  Aq`BAk|�AhĜAgƨAfv�Af$�Ae��Ad�yAc��AcdZA`��A^ȴA[�mAY�mAX�AU�AT��ASXAR1AP5?AMS�AJ��AF=qADffABffA@JA>A<��A;C�A9;dA8ZA7�
A7t�A6jA5��A5"�A4r�A3A3%A2jA1�#A1;dA0��A/��A/O�A-��A-
=A,r�A+�hA)|�A(�RA(z�A'\)A&^5A%��A"�!A!+A�A��Ar�A��A�AZA�RA�RAVA�yA�yA  A��AA�AJA�`A�-A~�A1'AA��A�A�A
=A��A\)AbNA ��@�V@�l�@�+@�V@�A�@�(�@�(�@��@��F@�{@��@���@�(�@�1'@� �@�I�@��@�5?@�h@�?}@���@�Z@��;@�K�@��@�~�@��@�@��@�h@���@�o@�5?@�D@�=q@�dZ@�E�@��@�S�@���@�M�@�`B@��@�ff@��#@�/@�\)@ҟ�@�$�@�Z@�l�@�@��/@�Ĝ@���@�Ĝ@�z�@˶F@��H@�O�@�hs@��#@ɲ-@ɡ�@ə�@�hs@ț�@��@���@ǝ�@�dZ@��@�E�@��#@�x�@Ĭ@�ƨ@�
=@�V@�bN@��@���@�l�@�~�@�-@�@�`B@��@�bN@���@��@�V@�@���@���@���@�~�@���@���@�V@�%@��@�Ĝ@��D@�j@��m@�C�@��@�O�@�9X@�b@��@�ƨ@�C�@�ȴ@�ff@��@���@�O�@��@� �@��@��@�C�@���@�5?@�hs@���@��@���@�j@�  @�
=@��!@�~�@�^5@�$�@���@���@��^@�%@��/@��j@���@��u@�1'@�l�@�@�ȴ@�v�@�@���@���@�@���@��@��D@�  @�dZ@�33@�
=@���@�^5@�$�@��@�G�@�I�@�(�@�1@��;@��F@���@���@�t�@���@�r�@���@���@��F@��@�@��@��@�&�@�?}@��@��7@�x�@�x�@�`B@�7L@�?}@��/@�z�@��m@�l�@�+@�o@���@�v�@�n�@�ff@�V@�V@�{@���@���@���@���@�O�@���@�Q�@�(�@��@��m@��@��@��;@��F@�l�@��H@�ȴ@���@�=q@��7@�&�@�%@��@��D@�z�@�A�@��@��w@���@�\)@�"�@��@���@�n�@�V@�^5@�5?@�{@��@�@���@�hs@�`B@�X@�7L@��@�V@��/@��@�`B@�/@�?}@�?}@�/@��@���@��u@�bN@�1'@��F@�t�@�t�@�l�@��@���@���@��R@��\@�E�@��@��@�@��T@��-@��-@���@�p�@�O�@�G�@�7L@��`@�z�@�  @�33@��R@�n�@�ff@�E�@�-@���@�@���@�p�@�`B@�X@�7L@�/@��@�E�@s��@_�&1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�~�A�~�A�z�A߃A߉7AߋDA߉7A߉7AߋDAߋDAߋDAߍPAߏ\AߋDA߃A�r�A�K�A�-A��mA޲-A�bNA�A�S�A٬A���AؑhA�n�A�33A��A�bA���A�(�A�%A�v�A��mA�1'A�ȴA�A�A��A�ȴA�ƨA���A�G�A�n�A�A�A��\A���A�|�A��TA��A���A�A�^5A�z�A�x�A� �A��A�^5A���A�~�A��A���A�A���A�M�A���A��`A��A�t�A��-A��-A��A��#A�l�A�jA���A�ĜA��A�S�A~��A}�Awx�At  Aq`BAk|�AhĜAgƨAfv�Af$�Ae��Ad�yAc��AcdZA`��A^ȴA[�mAY�mAX�AU�AT��ASXAR1AP5?AMS�AJ��AF=qADffABffA@JA>A<��A;C�A9;dA8ZA7�
A7t�A6jA5��A5"�A4r�A3A3%A2jA1�#A1;dA0��A/��A/O�A-��A-
=A,r�A+�hA)|�A(�RA(z�A'\)A&^5A%��A"�!A!+A�A��Ar�A��A�AZA�RA�RAVA�yA�yA  A��AA�AJA�`A�-A~�A1'AA��A�A�A
=A��A\)AbNA ��@�V@�l�@�+@�V@�A�@�(�@�(�@��@��F@�{@��@���@�(�@�1'@� �@�I�@��@�5?@�h@�?}@���@�Z@��;@�K�@��@�~�@��@�@��@�h@���@�o@�5?@�D@�=q@�dZ@�E�@��@�S�@���@�M�@�`B@��@�ff@��#@�/@�\)@ҟ�@�$�@�Z@�l�@�@��/@�Ĝ@���@�Ĝ@�z�@˶F@��H@�O�@�hs@��#@ɲ-@ɡ�@ə�@�hs@ț�@��@���@ǝ�@�dZ@��@�E�@��#@�x�@Ĭ@�ƨ@�
=@�V@�bN@��@���@�l�@�~�@�-@�@�`B@��@�bN@���@��@�V@�@���@���@���@�~�@���@���@�V@�%@��@�Ĝ@��D@�j@��m@�C�@��@�O�@�9X@�b@��@�ƨ@�C�@�ȴ@�ff@��@���@�O�@��@� �@��@��@�C�@���@�5?@�hs@���@��@���@�j@�  @�
=@��!@�~�@�^5@�$�@���@���@��^@�%@��/@��j@���@��u@�1'@�l�@�@�ȴ@�v�@�@���@���@�@���@��@��D@�  @�dZ@�33@�
=@���@�^5@�$�@��@�G�@�I�@�(�@�1@��;@��F@���@���@�t�@���@�r�@���@���@��F@��@�@��@��@�&�@�?}@��@��7@�x�@�x�@�`B@�7L@�?}@��/@�z�@��m@�l�@�+@�o@���@�v�@�n�@�ff@�V@�V@�{@���@���@���@���@�O�@���@�Q�@�(�@��@��m@��@��@��;@��F@�l�@��H@�ȴ@���@�=q@��7@�&�@�%@��@��D@�z�@�A�@��@��w@���@�\)@�"�@��@���@�n�@�V@�^5@�5?@�{@��@�@���@�hs@�`B@�X@�7L@��@�V@��/@��@�`B@�/@�?}@�?}@�/@��@���@��u@�bN@�1'@��F@�t�@�t�@�l�@��@���@���@��R@��\@�E�@��@��@�@��T@��-@��-@���@�p�@�O�@�G�@�7L@��`@�z�@�  @�33@��R@�n�@�ff@�E�@�-@���@�@���@�p�@�`B@�X@�7L@�/@��@�E�@s��@_�&1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B  B
��B  B  B
��B
��B  B
��B
��B
��B  B  B  B
��B
��B  BBB1BDB7LBn�B�=B�{B��B��B�'B�?B�XB�dB�qB�dB�FBv�B/B��BjB`BB|�B��B��B��B��B�B�^BDB^5BH�B	7B�`B�mB<j?��hB  BBBB��B�B�
BƨB�XB�B��B�BiyB[#BG�B7LB+B!�B,B�B%B
�B
�)B
�LB
��B
bNB
E�B
7LB
uB	��B	�yB	ɺB	�XB	�9B	�B	�B	��B	��B	��B	�{B	�B	u�B	bNB	VB	J�B	C�B	<jB	7LB	0!B	&�B	�B	JB��B�B�mB�B��B��BƨB�}B�jB�dB�^B�^B�XB�XB�RB�LB�LB�FB�?B�9B�-B�!B�B�B�B�B��B��B��B��B��B��B��B��B��B�bB��B��B�uB�\B�DB�B�B�B�+B�VB��B�9BB��B��B��B��B��B��BɺB��B{�BaHB[#B]/BiyBe`BaHBS�BS�BS�BXBXB]/BaHBo�B}�Bz�Bq�Bw�B�7B��B��B��B�B�!B�'B�'B�'B�3B�3B�3B�3B�3B�3B�3B�3B�9B�?B�9B�?B�RB�^B�?B�FB�RB�XB�wB��B��BB��B��B��B�wB�qB�jB�}B�}B��B��B��B��B��B��BBÖBǮB��B��B�B�
B�
B�B�#B�)B�5B�;B�BB�HB�`B�fB�mB�mB�B��B��B	B	B	B	B	
=B	DB	PB	VB	\B	oB	uB	�B	�B	�B	!�B	#�B	&�B	+B	.B	0!B	2-B	33B	49B	49B	5?B	6FB	6FB	9XB	B�B	J�B	N�B	S�B	W
B	\)B	_;B	bNB	dZB	ffB	gmB	hsB	l�B	q�B	t�B	w�B	z�B	|�B	}�B	�B	�1B	�=B	�=B	�JB	�\B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�-B	�3B	�9B	�?B	�?B	�LB	�XB	�XB	�^B	�^B	�dB	�jB	��B	B	ŢB	ǮB	ƨB	ǮB	ǮB	ȴB	ɺB	��B	��B	��B	��B	�B	�B	�#B	�#B	�#B	�B	�#B	�#B	�)B	�5B	�BB	�HB	�NB	�ZB	�`B	�`B	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
+B
PB
\B
\B
bB
bB
bB
bB
bB
bB
bB
bB
bB
bB
bB
bB
bB
bB
bB
bB
bB
bB
hB
hB
hB
hB
oB
oB
oB
oB
oB
oB
oB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
,�B
8�1111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B  B
��B  B  B
��B
��B  B
��B
��B
��B  B  B  B
��B
��B  BBB1BDB7LBn�B�=B�{B��B��B�'B�?B�XB�dB�qB�dB�FBv�B/B��BjB`BB|�B��B��B��B��B�B�^BDB^5BH�B	7B�`B�mB<j?��hB  BBBB��B�B�
BƨB�XB�B��B�BiyB[#BG�B7LB+B!�B,B�B%B
�B
�)B
�LB
��B
bNB
E�B
7LB
uB	��B	�yB	ɺB	�XB	�9B	�B	�B	��B	��B	��B	�{B	�B	u�B	bNB	VB	J�B	C�B	<jB	7LB	0!B	&�B	�B	JB��B�B�mB�B��B��BƨB�}B�jB�dB�^B�^B�XB�XB�RB�LB�LB�FB�?B�9B�-B�!B�B�B�B�B��B��B��B��B��B��B��B��B��B�bB��B��B�uB�\B�DB�B�B�B�+B�VB��B�9BB��B��B��B��B��B��BɺB��B{�BaHB[#B]/BiyBe`BaHBS�BS�BS�BXBXB]/BaHBo�B}�Bz�Bq�Bw�B�7B��B��B��B�B�!B�'B�'B�'B�3B�3B�3B�3B�3B�3B�3B�3B�9B�?B�9B�?B�RB�^B�?B�FB�RB�XB�wB��B��BB��B��B��B�wB�qB�jB�}B�}B��B��B��B��B��B��BBÖBǮB��B��B�B�
B�
B�B�#B�)B�5B�;B�BB�HB�`B�fB�mB�mB�B��B��B	B	B	B	B	
=B	DB	PB	VB	\B	oB	uB	�B	�B	�B	!�B	#�B	&�B	+B	.B	0!B	2-B	33B	49B	49B	5?B	6FB	6FB	9XB	B�B	J�B	N�B	S�B	W
B	\)B	_;B	bNB	dZB	ffB	gmB	hsB	l�B	q�B	t�B	w�B	z�B	|�B	}�B	�B	�1B	�=B	�=B	�JB	�\B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�-B	�3B	�9B	�?B	�?B	�LB	�XB	�XB	�^B	�^B	�dB	�jB	��B	B	ŢB	ǮB	ƨB	ǮB	ǮB	ȴB	ɺB	��B	��B	��B	��B	�B	�B	�#B	�#B	�#B	�B	�#B	�#B	�)B	�5B	�BB	�HB	�NB	�ZB	�`B	�`B	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
+B
PB
\B
\B
bB
bB
bB
bB
bB
bB
bB
bB
bB
bB
bB
bB
bB
bB
bB
bB
bB
bB
hB
hB
hB
hB
oB
oB
oB
oB
oB
oB
oB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
,�B
8�1111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.23 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140818                              AO  ARCAADJP                                                                    20181024140818    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140818  QCP$                G�O�G�O�G�O�F03E            AO  ARGQQCPL                                                                    20181024140818  QCF$                G�O�G�O�G�O�0               