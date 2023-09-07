CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:56Z creation      
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
_FillValue                    �@Argo profile    3.1 1.2 19500101000000  20181005191756  20181005191756  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL              &A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @���s|�1   @���`�V@5J~��"��d�A�7K�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                     &A   A   A   @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   BffB  B  B   B(  B0  B8  B@  BG��BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B���C   C  C  C  C  C
�C�C  C  C  C  C  C  C  C  CL  CN  CP  CQ�fCS�fCU�fCX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cu�fCw�fCz�C|  C~  C�  C�  C�  C�  C�  C��3C��3C�  C��C��C�  C��3C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C��C�  C��3C��3C��3C��3C�  C��C��C��C��C��C��C��3C��3C�  C�  C�  C��3C��C�  C��3C��3C��C��C�  C�  C�  C�  C�  C��3C��3C�  C��C�  C�  C�  C��3C�  C��3C�  C�  C��C�  C�  C�  C�  C��C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C��3C��3C�  C�  C�  C��C�  C�  C�  C�  C��C�  C��3C��3C��3C�  D   D � D  D� D��Dy�D��Ds3D  D� D��D� D  Dy�D  D�fD  Dy�D��D	� D
  D
y�D
��Dy�D�3Dy�D��D� D  D� DfD�fD  D� D  D�fDfD��D  Dy�D  D� D�D� D��D�fD  D� D�D� D  D�fD  D� D  D� D  D�fD  D� D  D� D��D� D fD � D!  D!y�D"  D"� D#  D#� D$  D$� D%  D%y�D%��D&� D&��D'� D(  D(� D)  D)y�D*fD*� D+  D+�fD,  D,y�D,��D-�fD-��D.� D/  D/� D/��D0y�D1fD1� D2  D2� D3  D3y�D4fD4� D5  D5� D5��D6� D6��D7�fD8  D8y�D9  D9� D:fD:y�D;  D;� D<fD<�fD=  D=� D>fD>� D?  D?� D?��D@y�DA  DA� DB  DBy�DCfDC� DD  DDy�DE  DE� DFfDFy�DG  DG� DH  DH� DI  DIy�DJfDJ�fDKfDK�fDL  DL�fDMfDMy�DM��DN� DO  DOy�DO��DP� DQfDQ�fDRfDR�fDS  DSy�DS��DTy�DU  DU�fDVfDV� DW  DW� DX  DX� DYfDY� DZ  DZ� DZ��D[s3D\  D\� D]fD]� D^  D^�fD^��D_� D_��D`� D`��Da� DbfDb� Db��Dc�fDd  Dd�fDe  De� De��Df� Df��Dg� Dh  Dh�fDifDi�fDi��Djy�Dk  Dk� Dk��Dl� DmfDm� Dn  Dn� Dn��Do�fDp  Dpy�DqfDq� Dr  Dr�fDsfDsy�Dt  Dt� Du  Du� Dv  Dv� Dw  Dw�fDw�fDy��D�4)1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @AG�@��
@��
A�A!�AA�Aa�A���A���A���A���A���A���A���A���B z�B�GBz�Bz�B z�B(z�B0z�B8z�B@z�BH{BPz�BXz�B`z�Bhz�Bpz�Bxz�B�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�
>B�
>B�
>C �C�C�C�C�C
8RC8RC�C�C�C�C�C�C�C�CL�CN�CP�CRCTCVCX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�CvCxCz8RC|�C~�C�\C�\C�\C�\C�\C��C��C�\C�)C�)C�\C��C�\C�)C�)C�)C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�)C�\C��C�\C�\C�\C�\C�\C�\C�\C��C�\C�)C�)C�\C��C��C��C��C�\C�)C�)C�)C�)C�)C�)C��C��C�\C�\C�\C��C�)C�\C��C��C�)C�)C�\C�\C�\C�\C�\C��C��C�\C�)C�\C�\C�\C��C�\C��C�\C�\C�)C�\C�\C�\C�\C�)C�)C�)C�)C�\C�\C�\C�\C�\C�\C�\C�\C�\C��C��C��C��C��C�\C�\C�\C�)C�\C�\C�\C�\C�)C�\C��C��C��C�\D �D ��D�D��DHD�HDHDz�D�D��DHD��D�D�HD�D�D�D�HD	HD	��D
�D
�HDHD�HD��D�HDHD��D�D��DD�D�D��D�D�DD�{D�D�HD�D��D{D��DHD�D�D��D{D��D�D�D�D��D�D��D�D�D�D��D�D��DHD��D D ��D!�D!�HD"�D"��D#�D#��D$�D$��D%�D%�HD&HD&��D'HD'��D(�D(��D)�D)�HD*D*��D+�D+�D,�D,�HD-HD-�D.HD.��D/�D/��D0HD0�HD1D1��D2�D2��D3�D3�HD4D4��D5�D5��D6HD6��D7HD7�D8�D8�HD9�D9��D:D:�HD;�D;��D<D<�D=�D=��D>D>��D?�D?��D@HD@�HDA�DA��DB�DB�HDCDC��DD�DD�HDE�DE��DFDF�HDG�DG��DH�DH��DI�DI�HDJDJ�DKDK�DL�DL�DMDM�HDNHDN��DO�DO�HDPHDP��DQDQ�DRDR�DS�DS�HDTHDT�HDU�DU�DVDV��DW�DW��DX�DX��DYDY��DZ�DZ��D[HD[z�D\�D\��D]D]��D^�D^�D_HD_��D`HD`��DaHDa��DbDb��DcHDc�Dd�Dd�De�De��DfHDf��DgHDg��Dh�Dh�DiDi�DjHDj�HDk�Dk��DlHDl��DmDm��Dn�Dn��DoHDo�Dp�Dp�HDqDq��Dr�Dr�DsDs�HDt�Dt��Du�Du��Dv�Dv��Dw�Dw�Dw�Dy�{D�8 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�ȴAŸRA�jA�~�A¶FA���A�VA��A�v�A�=qA�VA���A��wA���A�l�A�
=A��9A�ZA�?}A�;dA�&�A��A��A��A�A��A��RA��7A�XA�$�A�ĜA��A�v�A�33A��wA�G�A��`A��jA�A���A�bNA��hA�n�A�9XA��-A�n�A�;dA�VA��A��9A���A�`BA�E�A��A���A�l�A�Q�A�VA��-A���A�S�A��#A�p�A���A���A��A�5?A���A�A�A���A��7A��yA�l�A��`A�C�A��A���A�=qA�5?A��A���A���A��!A���A��
A���A�r�A�  A�A�z�A�$�A~��A}�A|��Az(�Aw�TAw"�Au�wAs��Aq��Ap�RApM�Ao��Ao��AoVAmG�Al^5Ak�Ajr�Ah�Ah�AhI�Ah�Ah�AgS�Af=qAex�Ae&�Ad�9Ac�;Ac�Ab�RAa�A_�A]�AZ$�AY%AW"�AU/AT�jAS��ASC�AQ�PANA�AL�DAKK�AK"�AI��AH�`AEO�ABQ�A@�A>ffA>1A<�!A9�-A7`BA7\)A7+A6  A4��A3S�A1��A0n�A/XA.�DA-t�A,�!A+VA'33A%hsA$v�A#A"�A!t�A v�A��A�HAA�A��A�Av�AVA��AAbA��A�AĜA��Al�A�A��A�jAz�A�A�A��AK�A;dA��A^5A~�A
=A
�`A
jA	O�A	"�AȴAffA1A��A�A�\AA�/A��A Z@�n�@�O�@�A�@�@��+@��@���@�I�@��!@��@��`@�@��T@�@�9X@�5?@���@�P@���@�`B@�D@�P@�^@߅@�7L@��@�@�`B@��@؛�@�&�@Ӆ@��H@�E�@�{@���@��T@��#@�/@ϕ�@�=q@�x�@���@�1'@�l�@��@�V@ɲ-@�z�@ǥ�@�@Ɵ�@�
=@�+@�1'@�bN@�bN@�1@��@�E�@Ų-@�O�@��`@��
@�C�@���@�|�@�J@���@�;d@��\@�O�@�bN@�;d@�-@��D@��@���@���@��P@�@�p�@�?}@��@���@�\)@�K�@�+@�@���@�^5@�-@�$�@��T@�@�`B@���@��j@�z�@�z�@�Ĝ@���@�I�@�(�@���@�33@�ff@�5?@���@���@�p�@�X@�G�@���@��/@���@��9@���@��@�r�@�j@�A�@��F@�C�@�
=@��@��+@�@�@��^@���@�x�@�&�@���@�Q�@�b@��F@���@���@���@��@��#@�@��-@��7@��@��@�1@���@�K�@�ȴ@���@�v�@��!@�~�@�$�@�{@�{@�{@�J@��@��T@�J@�-@�-@��^@�V@�Ĝ@���@��D@�bN@�(�@��;@��w@���@�l�@��@��!@��\@�v�@�ff@�5?@��T@���@���@��@�1'@��;@�dZ@��H@���@���@�n�@�M�@�=q@���@��7@�X@�O�@�/@��@��@���@��j@��@�I�@�9X@�1'@�(�@�1@��w@�|�@�;d@��y@���@��R@���@���@�^5@�@���@��@��`@�Ĝ@��@�z�@�Q�@�9X@�b@�  @��;@��w@��F@��P@�|�@�@���@���@��+@�=q@��#@��7@�O�@��/@��D@�Q�@�(�@��w@��@�C�@�o@��H@��R@��!@���@�M�@���@��7@�G�@�V@���@��@�(�@�b@���@�W?@y|1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�ȴAŸRA�jA�~�A¶FA���A�VA��A�v�A�=qA�VA���A��wA���A�l�A�
=A��9A�ZA�?}A�;dA�&�A��A��A��A�A��A��RA��7A�XA�$�A�ĜA��A�v�A�33A��wA�G�A��`A��jA�A���A�bNA��hA�n�A�9XA��-A�n�A�;dA�VA��A��9A���A�`BA�E�A��A���A�l�A�Q�A�VA��-A���A�S�A��#A�p�A���A���A��A�5?A���A�A�A���A��7A��yA�l�A��`A�C�A��A���A�=qA�5?A��A���A���A��!A���A��
A���A�r�A�  A�A�z�A�$�A~��A}�A|��Az(�Aw�TAw"�Au�wAs��Aq��Ap�RApM�Ao��Ao��AoVAmG�Al^5Ak�Ajr�Ah�Ah�AhI�Ah�Ah�AgS�Af=qAex�Ae&�Ad�9Ac�;Ac�Ab�RAa�A_�A]�AZ$�AY%AW"�AU/AT�jAS��ASC�AQ�PANA�AL�DAKK�AK"�AI��AH�`AEO�ABQ�A@�A>ffA>1A<�!A9�-A7`BA7\)A7+A6  A4��A3S�A1��A0n�A/XA.�DA-t�A,�!A+VA'33A%hsA$v�A#A"�A!t�A v�A��A�HAA�A��A�Av�AVA��AAbA��A�AĜA��Al�A�A��A�jAz�A�A�A��AK�A;dA��A^5A~�A
=A
�`A
jA	O�A	"�AȴAffA1A��A�A�\AA�/A��A Z@�n�@�O�@�A�@�@��+@��@���@�I�@��!@��@��`@�@��T@�@�9X@�5?@���@�P@���@�`B@�D@�P@�^@߅@�7L@��@�@�`B@��@؛�@�&�@Ӆ@��H@�E�@�{@���@��T@��#@�/@ϕ�@�=q@�x�@���@�1'@�l�@��@�V@ɲ-@�z�@ǥ�@�@Ɵ�@�
=@�+@�1'@�bN@�bN@�1@��@�E�@Ų-@�O�@��`@��
@�C�@���@�|�@�J@���@�;d@��\@�O�@�bN@�;d@�-@��D@��@���@���@��P@�@�p�@�?}@��@���@�\)@�K�@�+@�@���@�^5@�-@�$�@��T@�@�`B@���@��j@�z�@�z�@�Ĝ@���@�I�@�(�@���@�33@�ff@�5?@���@���@�p�@�X@�G�@���@��/@���@��9@���@��@�r�@�j@�A�@��F@�C�@�
=@��@��+@�@�@��^@���@�x�@�&�@���@�Q�@�b@��F@���@���@���@��@��#@�@��-@��7@��@��@�1@���@�K�@�ȴ@���@�v�@��!@�~�@�$�@�{@�{@�{@�J@��@��T@�J@�-@�-@��^@�V@�Ĝ@���@��D@�bN@�(�@��;@��w@���@�l�@��@��!@��\@�v�@�ff@�5?@��T@���@���@��@�1'@��;@�dZ@��H@���@���@�n�@�M�@�=q@���@��7@�X@�O�@�/@��@��@���@��j@��@�I�@�9X@�1'@�(�@�1@��w@�|�@�;d@��y@���@��R@���@���@�^5@�@���@��@��`@�Ĝ@��@�z�@�Q�@�9X@�b@�  @��;@��w@��F@��P@�|�@�@���@���@��+@�=q@��#@��7@�O�@��/@��D@�Q�@�(�@��w@��@�C�@�o@��H@��R@��!@���@�M�@���@��7@�G�@�V@���@��@�(�@�b@���@�W?@y|1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Bm�Bm�Bk�BcTBQ�B?}B�BB��BBDBbBoB{B�B7LB;dBB�BM�B\)BbNBhsBl�Bp�Bx�Bx�Bq�Bu�B� B�DB�uB��B��B�-B�?B�qBBɺB�#B�B�)B�;B�5B�NB�yB��B1B1B	7BJBJB�B!�B�B�B�B�B#�B(�B,B+B(�B+B(�B&�B �B�BhBJB
=B%BB��B�B��BĜB�jB�?B�B	7B
��B
��B
�B
�ZB
�HB
�;B
�)B
��B
B
�B
��B
�B
|�B
t�B
e`B
T�B
N�B
E�B
7LB
)�B
#�B
!�B
�B
�B
�B
VB
%B
B	��B	�B	�B	�B	�sB	�mB	�TB	�)B	�B	��B	��B	��B	ĜB	��B	�LB	��B	��B	�B	|�B	q�B	e`B	bNB	\)B	W
B	K�B	:^B	/B	%�B	#�B	�B	hB��B�fB�#B��B��B��B��BƨBŢBĜB��B�wB�LB�B��B��B��B��B��B�hB�7B�B�B|�By�Bv�Bs�Bo�Bl�BffBhsBhsBiyBjBjBk�Bl�Bk�BjBjBhsBgmBgmBgmBgmBiyBk�BiyBiyBgmBgmBe`BcTB`BBffBiyBiyBffBe`BcTBaHB`BB]/BYBVBT�BYBXBXB[#B\)B^5BdZBhsBhsBn�Bp�Bt�Bt�Bt�Bs�Bt�Bt�Bq�Bn�Bl�Bl�Bm�Bl�Bo�Bn�Bn�Bm�Bk�BgmBffBe`BdZBbNBe`BgmBiyBjBjBk�Bk�BjBl�Bn�Bp�Bv�Bv�Bv�Bw�Bz�Bz�Bz�B{�B}�B�%B�DB�\B�uB��B��B��B��B�B�B�B�B�!B�3B�9B�LB�^B�dBƨB��B��B��B��B��B�
B�BB�TB�`B�`B�ZB�fB�fB�mB�B�B�B�B��B��B��B��B��B��B	B	B	B	PB	\B	hB	uB	�B	�B	!�B	#�B	%�B	+B	-B	-B	.B	/B	2-B	7LB	8RB	;dB	<jB	<jB	=qB	>wB	?}B	?}B	?}B	@�B	E�B	H�B	J�B	K�B	N�B	S�B	W
B	XB	XB	ZB	]/B	_;B	aHB	bNB	cTB	dZB	hsB	jB	o�B	r�B	r�B	r�B	s�B	u�B	w�B	x�B	w�B	w�B	y�B	y�B	z�B	~�B	� B	� B	�B	�B	�%B	�+B	�7B	�JB	�\B	�bB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�'B	�9B	�FB	�RB	�XB	�XB	�^B	�qB	�wB	��B	��B	��B	B	B	ÖB	ĜB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�#B	�)B	�5B	�;B	�;B	�;B	�;B	�BB	�BB	�BB	�HB	�HB	�NB	�ZB	�mB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
[B
�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 Bm�Bm�Bk�BcTBQ�B?}B�BB��BBDBbBoB{B�B7LB;dBB�BM�B\)BbNBhsBl�Bp�Bx�Bx�Bq�Bu�B� B�DB�uB��B��B�-B�?B�qBBɺB�#B�B�)B�;B�5B�NB�yB��B1B1B	7BJBJB�B!�B�B�B�B�B#�B(�B,B+B(�B+B(�B&�B �B�BhBJB
=B%BB��B�B��BĜB�jB�?B�B	7B
��B
��B
�B
�ZB
�HB
�;B
�)B
��B
B
�B
��B
�B
|�B
t�B
e`B
T�B
N�B
E�B
7LB
)�B
#�B
!�B
�B
�B
�B
VB
%B
B	��B	�B	�B	�B	�sB	�mB	�TB	�)B	�B	��B	��B	��B	ĜB	��B	�LB	��B	��B	�B	|�B	q�B	e`B	bNB	\)B	W
B	K�B	:^B	/B	%�B	#�B	�B	hB��B�fB�#B��B��B��B��BƨBŢBĜB��B�wB�LB�B��B��B��B��B��B�hB�7B�B�B|�By�Bv�Bs�Bo�Bl�BffBhsBhsBiyBjBjBk�Bl�Bk�BjBjBhsBgmBgmBgmBgmBiyBk�BiyBiyBgmBgmBe`BcTB`BBffBiyBiyBffBe`BcTBaHB`BB]/BYBVBT�BYBXBXB[#B\)B^5BdZBhsBhsBn�Bp�Bt�Bt�Bt�Bs�Bt�Bt�Bq�Bn�Bl�Bl�Bm�Bl�Bo�Bn�Bn�Bm�Bk�BgmBffBe`BdZBbNBe`BgmBiyBjBjBk�Bk�BjBl�Bn�Bp�Bv�Bv�Bv�Bw�Bz�Bz�Bz�B{�B}�B�%B�DB�\B�uB��B��B��B��B�B�B�B�B�!B�3B�9B�LB�^B�dBƨB��B��B��B��B��B�
B�BB�TB�`B�`B�ZB�fB�fB�mB�B�B�B�B��B��B��B��B��B��B	B	B	B	PB	\B	hB	uB	�B	�B	!�B	#�B	%�B	+B	-B	-B	.B	/B	2-B	7LB	8RB	;dB	<jB	<jB	=qB	>wB	?}B	?}B	?}B	@�B	E�B	H�B	J�B	K�B	N�B	S�B	W
B	XB	XB	ZB	]/B	_;B	aHB	bNB	cTB	dZB	hsB	jB	o�B	r�B	r�B	r�B	s�B	u�B	w�B	x�B	w�B	w�B	y�B	y�B	z�B	~�B	� B	� B	�B	�B	�%B	�+B	�7B	�JB	�\B	�bB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�'B	�9B	�FB	�RB	�XB	�XB	�^B	�qB	�wB	��B	��B	��B	B	B	ÖB	ĜB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�#B	�)B	�5B	�;B	�;B	�;B	�;B	�BB	�BB	�BB	�HB	�HB	�NB	�ZB	�mB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
[B
�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.12 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191756                              AO  ARCAADJP                                                                    20181005191756    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191756  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191756  QCF$                G�O�G�O�G�O�8000            