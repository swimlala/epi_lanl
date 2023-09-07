CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:13Z creation      
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
_FillValue                 �  A<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C0   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^|   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  fH   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h<   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �T   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �$   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �(   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �,   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20181005191713  20181005191713  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               lA   AO  6557                            2B  A   APEX                            7468                            062512                          846 @��d����1   @��e)�,@4� ě���dQ$�/1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      lA   A   A   @333@y��@���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BHffBP  BW��B_��Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C�fC�fC  C  C  C  C  C�C�C �C"  C$  C&�C(33C*  C+�fC.  C0  C2  C4�C6  C8  C:  C<  C>  C@�CB  CD  CF  CG�fCJ  CL  CN  CO�fCR  CT  CV  CW�fCZ  C\  C]�fC`  Ca�fCc�fCf  Ch  Cj  Ck�fCn�Co�fCq��Cs�fCv�Cx�Cz�C|  C~  C�  C�  C��fC��C��C�  C�  C��3C��3C�  C��C�  C�  C�  C��C��3C�  C�  C��3C��3C��3C��3C��3C��C�  C�  C��3C��3C�  C�  C��C�  C�  C�  C�  C��3C��C��C�  C�  C��3C��3C��3C��3C�  C��C��3C�  C�  C��3C��3C�  C�  C��C��C��C�  C�  C��C�  C��C��C�  C��C�  C��3C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C��C�  C��3C�  C��C�  C�  C��3C�  C��3C��3C��3C��3C��3C��3C�  C�  C�  C��3C��3C�  C�  C�  C��C��C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3D � D  D� D  D� DfD�fDfD�fDfD� D  D� D  D� D��Dy�D	  D	� D
  D
� D  D� D  Dy�D  D� D��Dy�D��D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� DfD�fD  D� DfD� D��D� DfD� D  Dy�D  D�fD  D� D  D� D   D �fD!  D!� D"  D"�fD#fD#� D$  D$� D$��D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,fD,�fD-fD-� D.  D.y�D.��D/� D0fD0� D1  D1� D2fD2� D3  D3� D3��D4y�D5  D5� D6  D6� D7  D7� D8  D8� D9  D9�fD:fD:� D:��D;y�D<  D<� D=  D=� D>  D>� D?  D?� D@fD@�fDAfDA� DA��DB� DC  DCy�DC��DD� DE  DE� DF  DF� DG  DG� DH  DHy�DI  DI� DJ  DJ�fDK  DK� DLfDL�fDMfDM� DN  DN� DOfDO�fDP  DP� DP��DQy�DR  DR� DS  DS�fDT  DT� DUfDU� DV  DV� DW  DWy�DX  DXy�DX��DY� DZfDZ�fD[  D[y�D\  D\�fD]fD]� D]��D^�fD_  D_� D`fD`�fDa  Da� Da��Dby�Dc  Dcy�Dd  Dd�fDe  De� Df  Df� Dg  Dgy�Dh  Dh� Dh��Diy�Dj  Dj��Dk  Dk� Dl�Dl� Dm  Dm� Dn  Dny�Dn��Do� DpfDp�fDq  Dq�fDrfDr�fDsfDs�fDt  Dt� Du  Duy�Du��Dvy�Dw  Dw� Dw��Dx@ Dy��D�/
1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @A�@�(�@�(�A�A#�AC�Ac�A��
A��
A��
A��
A��
A��
A��
A��
B �B�B�B�B �B(�B0�B8�B@�BIQ�BP�BX�B`�Bh�Bp�Bx�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B���B���B���B�B�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�C :�C:�C:�C:�C:�C
:�C:�C!GC!GC:�C:�C:�C:�C:�CT{CT{C T{C":�C$:�C&T{C(nC*:�C,!GC.:�C0:�C2:�C4T{C6:�C8:�C::�C<:�C>:�C@T{CB:�CD:�CF:�CH!GCJ:�CL:�CN:�CP!GCR:�CT:�CV:�CX!GCZ:�C\:�C^!GC`:�Cb!GCd!GCf:�Ch:�Cj:�Cl!GCnT{Cp!GCr�Ct!GCvT{CxT{CzT{C|:�C~:�C�qC�qC��C�*>C�*>C�qC�qC��C��C�qC�*>C�qC�qC�qC�*>C��C�qC�qC��C��C��C��C��C�*>C�qC�qC��C��C�qC�qC�*>C�qC�qC�qC�qC��C�*>C�*>C�qC�qC��C��C��C��C�qC�*>C��C�qC�qC��C��C�qC�qC�*>C�*>C�*>C�qC�qC�*>C�qC�*>C�*>C�qC�*>C�qC��C�qC�qC��C�qC�qC�qC�qC�qC�qC�qC��C��C�qC�qC�qC�*>C�qC��C�qC�*>C�qC�qC��C�qC��C��C��C��C��C��C�qC�qC�qC��C��C�qC�qC�qC�*>C�*>C�qC�qC�qC��C�qC�*>C�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC��D RD ��D�D��D�D��DD�DD�DD��D�D��D�D��DRD�RD	�D	��D
�D
��D�D��D�D�RD�D��DRD�RDRD��D�D��D�D��D�D��D�D��D�D��D�D�D�D��DD�D�D��DD��DRD��DD��D�D�RD�D�D�D��D�D��D �D �D!�D!��D"�D"�D#D#��D$�D$��D%RD%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,D,�D-D-��D.�D.�RD/RD/��D0D0��D1�D1��D2D2��D3�D3��D4RD4�RD5�D5��D6�D6��D7�D7��D8�D8��D9�D9�D:D:��D;RD;�RD<�D<��D=�D=��D>�D>��D?�D?��D@D@�DADA��DBRDB��DC�DC�RDDRDD��DE�DE��DF�DF��DG�DG��DH�DH�RDI�DI��DJ�DJ�DK�DK��DLDL�DMDM��DN�DN��DODO�DP�DP��DQRDQ�RDR�DR��DS�DS�DT�DT��DUDU��DV�DV��DW�DW�RDX�DX�RDYRDY��DZDZ�D[�D[�RD\�D\�D]D]��D^RD^�D_�D_��D`D`�Da�Da��DbRDb�RDc�Dc�RDd�Dd�De�De��Df�Df��Dg�Dg�RDh�Dh��DiRDi�RDj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn�RDoRDo��DpDp�Dq�Dq�DrDr�DsDs�Dt�Dt��Du�Du�RDvRDv�RDw�Dw��DxRDxN�Dy�=D�6f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��;A��/A��;A��;A��;A��HA��TA��;A��HA��TA��TA��mA��yA��mA��TA��#A��
A��A��TA��mA��yA��A���Aܥ�Aܟ�A�+A��yA��A�XAǛ�A�&�A�ZA�A�?}AÑhA¡�A�$�A���A�C�A���A���A��9A��A�XA�/A���A��
A��;A�`BA�r�A�9XA�I�A��hA�ffA�XA�-A��A��DA�A�  A��A���A�{A��TA�z�A�p�A�`BA�5?A���A�\)A�{A��jA�7LA�JA��mA��TA��-A��A�oA��
A�K�A��-A���A�&�A�K�A�ZA�wA|�\Ay��Aw��AvjAuAu�Ar��Ap��Am�TAkAi�^Ag`BAeAd �A^�!A[7LAX�AWdZAUAT�DAR5?AN��AL�DAI�
AH$�AGoAE�-AB��A@�A?XA>JA>A>bA=��A=��A=oA<M�A;K�A:~�A9��A7\)A5�
A1`BA/�A.��A-��A,��A+�TA*��A);dA(~�A'�#A'C�A&��A%�A%`BA$��A#��A"�RA"E�A!p�A!
=A �A�A�7A;dA�A��AI�A��A�jA��A�A�7A�A`BA�^AdZA�Av�AbNAA�A  Al�A�!A�7A�A��AO�A?}A�RAffAJA��A�!A��A%AA�A�-A
��A
��A
Q�A
  A	+An�AbA�^A�A�A�`A�wAK�A��A�FA%@�ff@�bN@�n�@�Z@�{@�9X@��@���@��@���@���@�@��@�Z@�(�@�1@��@�(�@�R@��#@��@�9@�1@߾w@���@���@�O�@�&�@��m@߅@�|�@�t�@�l�@�t�@�\)@޸R@�J@݁@��@ڇ+@���@�ff@��@���@�;d@��@љ�@�%@��@϶F@�K�@�"�@���@���@�`B@�%@�I�@�V@�b@��;@�|�@�~�@��@Ų-@ř�@ũ�@ř�@őh@ŉ7@�p�@�z�@�1@��;@ÍP@�S�@��@�E�@�@��@��@���@�7L@� �@��@�;d@�
=@���@�ff@�/@�1'@��w@��R@�V@�@���@�x�@�?}@�7L@��/@�1@�dZ@�;d@���@��@��w@��@��R@���@���@���@�K�@�ff@���@��!@��^@�1@��m@��m@��@���@���@�
=@��-@��j@���@��u@�t�@�\)@�j@�A�@�&�@��-@�n�@���@��#@��-@��@�z�@�bN@�1@��@��@���@��@�ff@��@�-@��@�C�@���@���@��@�@��@��y@��@���@��R@�~�@�^5@��T@��h@�G�@��@��@��T@��h@���@���@���@��7@�`B@�&�@���@��j@��D@�(�@���@���@�\)@�
=@�V@��@��@���@��@��
@���@���@���@�t�@���@���@�~�@��@���@��D@�A�@�+@���@�v�@��@���@��@���@�1@�t�@�+@���@�E�@�E�@��@��@��T@�@���@��-@��7@�`B@�`B@��@���@�Q�@�1@�1@�1@�1@�1@�  @��@��
@��
@���@�
=@���@��R@���@�~�@�n�@�^5@�M�@�E�@�=q@�$�@��@���@�O�@�7L@�/@��9@�j@�  @���@��@�C�@�;d@�33@�33@�33@�"�@�o@���@�v�@�5?@��^@���@�(�@���@��w@���@�l�@�K�@�;d@��@���@�n�@�=q@�@��@��@��T@���@��^@�hs@�G�@��j@�9X@�@�;@�;@�;@+@~�+@~5?@}@}�=@o�41111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��;A��/A��;A��;A��;A��HA��TA��;A��HA��TA��TA��mA��yA��mA��TA��#A��
A��A��TA��mA��yA��A���Aܥ�Aܟ�A�+A��yA��A�XAǛ�A�&�A�ZA�A�?}AÑhA¡�A�$�A���A�C�A���A���A��9A��A�XA�/A���A��
A��;A�`BA�r�A�9XA�I�A��hA�ffA�XA�-A��A��DA�A�  A��A���A�{A��TA�z�A�p�A�`BA�5?A���A�\)A�{A��jA�7LA�JA��mA��TA��-A��A�oA��
A�K�A��-A���A�&�A�K�A�ZA�wA|�\Ay��Aw��AvjAuAu�Ar��Ap��Am�TAkAi�^Ag`BAeAd �A^�!A[7LAX�AWdZAUAT�DAR5?AN��AL�DAI�
AH$�AGoAE�-AB��A@�A?XA>JA>A>bA=��A=��A=oA<M�A;K�A:~�A9��A7\)A5�
A1`BA/�A.��A-��A,��A+�TA*��A);dA(~�A'�#A'C�A&��A%�A%`BA$��A#��A"�RA"E�A!p�A!
=A �A�A�7A;dA�A��AI�A��A�jA��A�A�7A�A`BA�^AdZA�Av�AbNAA�A  Al�A�!A�7A�A��AO�A?}A�RAffAJA��A�!A��A%AA�A�-A
��A
��A
Q�A
  A	+An�AbA�^A�A�A�`A�wAK�A��A�FA%@�ff@�bN@�n�@�Z@�{@�9X@��@���@��@���@���@�@��@�Z@�(�@�1@��@�(�@�R@��#@��@�9@�1@߾w@���@���@�O�@�&�@��m@߅@�|�@�t�@�l�@�t�@�\)@޸R@�J@݁@��@ڇ+@���@�ff@��@���@�;d@��@љ�@�%@��@϶F@�K�@�"�@���@���@�`B@�%@�I�@�V@�b@��;@�|�@�~�@��@Ų-@ř�@ũ�@ř�@őh@ŉ7@�p�@�z�@�1@��;@ÍP@�S�@��@�E�@�@��@��@���@�7L@� �@��@�;d@�
=@���@�ff@�/@�1'@��w@��R@�V@�@���@�x�@�?}@�7L@��/@�1@�dZ@�;d@���@��@��w@��@��R@���@���@���@�K�@�ff@���@��!@��^@�1@��m@��m@��@���@���@�
=@��-@��j@���@��u@�t�@�\)@�j@�A�@�&�@��-@�n�@���@��#@��-@��@�z�@�bN@�1@��@��@���@��@�ff@��@�-@��@�C�@���@���@��@�@��@��y@��@���@��R@�~�@�^5@��T@��h@�G�@��@��@��T@��h@���@���@���@��7@�`B@�&�@���@��j@��D@�(�@���@���@�\)@�
=@�V@��@��@���@��@��
@���@���@���@�t�@���@���@�~�@��@���@��D@�A�@�+@���@�v�@��@���@��@���@�1@�t�@�+@���@�E�@�E�@��@��@��T@�@���@��-@��7@�`B@�`B@��@���@�Q�@�1@�1@�1@�1@�1@�  @��@��
@��
@���@�
=@���@��R@���@�~�@�n�@�^5@�M�@�E�@�=q@�$�@��@���@�O�@�7L@�/@��9@�j@�  @���@��@�C�@�;d@�33@�33@�33@�"�@�o@���@�v�@�5?@��^@���@�(�@���@��w@���@�l�@�K�@�;d@��@���@�n�@�=q@�@��@��@��T@���@��^@�hs@�G�@��j@�9X@�@�;@�;@�;@+@~�+@~5?@}@}�=@o�41111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B0!B0!B0!B0!B0!B0!B0!B0!B0!B0!B0!B0!B0!B0!B0!B0!B0!B0!B0!B0!B0!B0!B/B/B.B!�B��B��B&�B6FB:^BA�BF�BXB]/Be`BhsBt�By�B~�B�B�JB�PB�bB��B��B��B��B��B�VB�+Bz�Bk�B`BBXBN�BE�B9XB2-B'�B�B
=B�B�)B��B�qB�!B��B�7B�B|�By�Bw�Bu�Bq�BhsBYB;dB!�BPB
��B
�`B
B
�RB
��B
~�B
XB
B�B
2-B
�B
�B
hB
PB	��B	�B	�B	ȴB	�dB	�B	��B	�{B	o�B	W
B	D�B	?}B	6FB	-B	 �B	bB	B��B�B�B�TB�)B�B�B��B�B�5B�BB�;B�5B�)B�B�B��B��BǮB�^B�-B�B�B��B��B��B��B�oB�hB�\B�PB�DB�7B�1B�%B�B�B�B�B� B~�B}�B|�B|�B{�By�Bx�Bw�Bx�B�B�B�B�B�+B�%B�B�B�B�B� B|�Bv�Bt�BjBhsBiyBjBm�Bm�Bm�Bl�BjBhsBhsBe`BcTBdZBe`BffBgmBm�Bn�Bn�Bn�Bm�Bp�Bq�Bq�Bq�Br�Bv�Bv�Bu�Bt�Bs�Bq�Bp�Bn�Bn�Bn�Bn�Bm�Bl�Bm�Bo�Bo�Bo�Bo�Bm�Bq�Br�Bu�Bx�Bz�B� B�%B�1B�PB�oB��B��B��B��B��B��B��B��B��B��B��B��B�B�!B�-B�'B�'B�'B�-B�9B�XB�wB�}B��B��B��BÖBĜBĜBĜB��B��B��B��B��B��B��B��B��B��B��B��B��B�
B�B�B�B�B�#B�BB�HB�NB�NB�NB�`B�B�B�B�B��B��B��B	B	B	
=B	JB	VB	\B	hB	oB	oB	uB	�B	�B	�B	�B	 �B	�B	"�B	.B	8RB	?}B	A�B	@�B	>wB	<jB	9XB	<jB	;dB	:^B	;dB	;dB	;dB	<jB	?}B	H�B	L�B	L�B	M�B	R�B	ZB	bNB	bNB	iyB	o�B	t�B	w�B	u�B	m�B	iyB	iyB	hsB	iyB	k�B	o�B	q�B	q�B	s�B	u�B	y�B	� B	�B	�B	�B	�B	�1B	�=B	�JB	�\B	�oB	�uB	��B	��B	��B	��B	��B	��B	�B	�!B	�-B	�9B	�9B	�9B	�?B	�FB	�LB	�RB	�XB	�^B	�jB	�qB	�wB	�}B	�}B	��B	ÖB	ÖB	ĜB	ÖB	B	ŢB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�#B	�)B	�)B	�5B	�;B	�BB	�BB	�BB	�HB	�ZB	�`B	�`B	�fB	�mB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B
�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 B0!B0!B0!B0!B0!B0!B0!B0!B0!B0!B0!B0!B0!B0!B0!B0!B0!B0!B0!B0!B0!B0!B/B/B.B!�B��B��B&�B6FB:^BA�BF�BXB]/Be`BhsBt�By�B~�B�B�JB�PB�bB��B��B��B��B��B�VB�+Bz�Bk�B`BBXBN�BE�B9XB2-B'�B�B
=B�B�)B��B�qB�!B��B�7B�B|�By�Bw�Bu�Bq�BhsBYB;dB!�BPB
��B
�`B
B
�RB
��B
~�B
XB
B�B
2-B
�B
�B
hB
PB	��B	�B	�B	ȴB	�dB	�B	��B	�{B	o�B	W
B	D�B	?}B	6FB	-B	 �B	bB	B��B�B�B�TB�)B�B�B��B�B�5B�BB�;B�5B�)B�B�B��B��BǮB�^B�-B�B�B��B��B��B��B�oB�hB�\B�PB�DB�7B�1B�%B�B�B�B�B� B~�B}�B|�B|�B{�By�Bx�Bw�Bx�B�B�B�B�B�+B�%B�B�B�B�B� B|�Bv�Bt�BjBhsBiyBjBm�Bm�Bm�Bl�BjBhsBhsBe`BcTBdZBe`BffBgmBm�Bn�Bn�Bn�Bm�Bp�Bq�Bq�Bq�Br�Bv�Bv�Bu�Bt�Bs�Bq�Bp�Bn�Bn�Bn�Bn�Bm�Bl�Bm�Bo�Bo�Bo�Bo�Bm�Bq�Br�Bu�Bx�Bz�B� B�%B�1B�PB�oB��B��B��B��B��B��B��B��B��B��B��B��B�B�!B�-B�'B�'B�'B�-B�9B�XB�wB�}B��B��B��BÖBĜBĜBĜB��B��B��B��B��B��B��B��B��B��B��B��B��B�
B�B�B�B�B�#B�BB�HB�NB�NB�NB�`B�B�B�B�B��B��B��B	B	B	
=B	JB	VB	\B	hB	oB	oB	uB	�B	�B	�B	�B	 �B	�B	"�B	.B	8RB	?}B	A�B	@�B	>wB	<jB	9XB	<jB	;dB	:^B	;dB	;dB	;dB	<jB	?}B	H�B	L�B	L�B	M�B	R�B	ZB	bNB	bNB	iyB	o�B	t�B	w�B	u�B	m�B	iyB	iyB	hsB	iyB	k�B	o�B	q�B	q�B	s�B	u�B	y�B	� B	�B	�B	�B	�B	�1B	�=B	�JB	�\B	�oB	�uB	��B	��B	��B	��B	��B	��B	�B	�!B	�-B	�9B	�9B	�9B	�?B	�FB	�LB	�RB	�XB	�^B	�jB	�qB	�wB	�}B	�}B	��B	ÖB	ÖB	ĜB	ÖB	B	ŢB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�#B	�)B	�)B	�5B	�;B	�BB	�BB	�BB	�HB	�ZB	�`B	�`B	�fB	�mB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B
�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.23 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191713                              AO  ARCAADJP                                                                    20181005191713    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191713  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191713  QCF$                G�O�G�O�G�O�8000            