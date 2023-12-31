CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2017-07-29T17:02:48Z creation      
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
resolution        :�o     �  q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �|   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �|   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �0   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �4   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �D   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �H   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �L   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20170729170248  20190405100805  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  5287                            2C  D   APEX                            6529                            072314                          846 @�+<T�1   @��lf@,ě��S��dsKƧ�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�33B�33B�33B�33B���B�  B���B���B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C�C  C  C  C�fC"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt� Dy3D�3D�9�D��fD���D�  D�@ D�y�D�� D�fD�@ D�l�D���D��D�L�Dڌ�D���D���D�9�D�y�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�ff@�33A	��A)��AI��Ai��A���A���A���A���A���A���A���A���BffB
ffBffBffB"ffB*ffB2ffB:ffBBffBJffBRffBZffBbffBjffBrffBzffB�33B�33B�33B�33B�ffB�ffB�ffB�ffB�  B�33B���B�  B�33B�33B�33B�  B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33C ��C��C��C��C��C
��C��C��C��C��C��C��C�4C��C��C��C � C"��C$��C&��C(��C*��C,��C.��C0��C2��C4��C6��C8��C:��C<��C>��C@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~��C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�Y�C�L�C�L�C�Y�C�Y�C�L�C�L�C�Y�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�@ C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�D &fD �fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD	&fD	�fD
&fD
�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD &fD �fD!&fD!�fD"&fD"�fD#&fD#�fD$&fD$�fD%&fD%�fD&&fD&�fD'&fD'�fD(&fD(�fD)&fD)�fD*&fD*�fD+&fD+�fD,&fD,�fD-&fD-�fD.&fD.�fD/&fD/�fD0&fD0�fD1&fD1�fD2&fD2�fD3&fD3�fD4&fD4�fD5&fD5�fD6&fD6�fD7&fD7�fD8&fD8�fD9&fD9�fD:&fD:�fD;&fD;�fD<&fD<�fD=&fD=�fD>&fD>�fD?&fD?�fD@&fD@�fDA&fDA�fDB&fDB�fDC&fDC�fDD&fDD�fDE&fDE�fDF&fDF�fDG&fDG�fDH&fDH�fDI&fDI�fDJ&fDJ�fDK&fDK�fDL&fDL�fDM&fDM�fDN&fDN�fDO&fDO�fDP&fDP�fDQ&fDQ�fDR&fDR�fDS&fDS�fDT&fDT�fDU&fDU�fDV&fDV�fDW&fDW�fDX&fDX�fDY&fDY�fDZ&fDZ�fD[&fD[�fD\&fD\�fD]&fD]�fD^&fD^�fD_&fD_�fD`&fD`�fDa&fDa�fDb&fDb�fDc&fDc�fDd&fDd�fDe&fDe�fDf&fDf�fDg&fDg�fDh&fDh�fDi&fDi�fDj&fDj�fDk&fDk�fDl&fDl�fDm&fDm�fDn&fDn�fDo&fDo�fDp&fDp�fDq&fDq�fDr&fDr�fDs&fDs�fDt&fDt�fDt�fDy9�D�fD�L�D���D�� D�3D�S3D���D��3D��D�S3D�� D�� D�,�D�` Dڠ D�� D� D�L�D��D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�^5A�ffA�\)A�S�A�S�A�A�A�=qAڡ�A�VA�;dA�$�A��A�A��A��yA��;A���A�ȴA�AٸRAٙ�AمA�ffA��A��A��mA��yA��A��A���A���A�1A��A�&�A��A���A�|�A��HAө�Aк^A�;dAβ-AΓuA��A�VA�I�A�7LA��A��A�7LA��A�ƨA�VAŴ9A�XA���A�O�A���A�XA�S�A��A��A�v�A�  A���A���A�{A��hA���A��;A� �A���A��A��/A��wA�ƨA�Q�A�Q�A�Q�A���A�hsA�I�A���A��A��
A��RA�\)A�E�A��A�~�A��wA���A�%A�JA��A��A��A� �A}�TAzv�Aw��Atn�Ar�/An$�Al  AhI�Aa|�A_�A^1'A[�AXJAU��AS�AQS�AO�AM�AK�AF��AB��A?O�A<��A<5?A;hsA:�A9��A7XA4��A3�
A3?}A2�\A2  A1oA/A.��A.1'A-VA+�A+oA*�+A*=qA)�^A(�A(r�A({A't�A&v�A%��A$A�A#�A"bNA!�A!K�A!�A �HA ZA�Ap�A �A?}A�jA-A"�A��A1A�RA��A��AA�A��A�#A�^Ap�AdZA|�AC�A��A  At�A�Av�A�Ax�A�A��A�A�A��A��AffA�A"�A�yA��Al�A&�Ar�A�7A`BA	��A�7A;dA��A�AbNAM�A�A;dAA�jA^5A1'A��A�A ��A ��A �/A Z@��y@��^@�b@��@��P@�-@��@�P@�K�@��@��@�-@�`B@��@�A�@���@�w@ꗍ@��@�7@�@��@�"�@�R@噚@�G�@�%@��/@�A�@��@���@�E�@�&�@�  @�;d@���@އ+@�5?@�M�@�$�@���@�ff@��@�
=@�M�@݉7@���@܃@��;@���@�n�@�E�@�J@�G�@���@ؼj@�Z@�V@���@�j@��@�1'@���@�Q�@ו�@�
=@�V@�M�@�~�@�M�@�J@և+@թ�@Դ9@�1'@���@�S�@�"�@���@�^5@�-@�=q@��@�G�@���@�Z@���@�n�@��@̓u@�Q�@��m@˅@���@�E�@��@�@���@��#@�7L@Ȭ@ț�@�bN@���@�dZ@�@�~�@��@Ĵ9@Õ�@��@°!@��@��-@���@��h@�X@��j@���@�Z@� �@��
@�l�@��@��@���@���@���@��@��`@��9@��u@�1'@��;@�|�@�"�@���@���@�5?@��@���@��-@�`B@�&�@�r�@��F@���@�ff@�J@���@�O�@���@���@��9@���@��u@��@�r�@�Z@�A�@� �@��m@�+@���@�E�@���@��^@��^@��h@��@��j@�j@�9X@��@�;d@�o@��@�n�@�J@��7@�&�@���@���@��9@���@�j@�9X@�  @��w@��P@�|�@�t�@�dZ@�dZ@�\)@�S�@�C�@�+@�"�@���@�{@���@�`B@��@�j@�  @�|�@�33@��@���@�~�@�E�@��7@��u@��@���@�+@��y@��!@�@�?}@��@��u@�I�@�A�@�  @�ƨ@�l�@�l�@�l�@�;d@��@�
=@��\@���@�`B@��@��u@��@�(�@���@���@�l�@�o@���@�~�@�@���@��h@��j@��u@�I�@�1'@� �@�1@���@�C�@�o@�ȴ@�E�@�@���@��7@�x�@�hs@�G�@���@��`@���@��j@��@��D@�
=@���@��@� �@t��@m�h@e�@^V@V�@N�y@F$�@=�@8��@17L@+@&E�@ 1'@ƨ@@~�@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�^5A�ffA�\)A�S�A�S�A�A�A�=qAڡ�A�VA�;dA�$�A��A�A��A��yA��;A���A�ȴA�AٸRAٙ�AمA�ffA��A��A��mA��yA��A��A���A���A�1A��A�&�A��A���A�|�A��HAө�Aк^A�;dAβ-AΓuA��A�VA�I�A�7LA��A��A�7LA��A�ƨA�VAŴ9A�XA���A�O�A���A�XA�S�A��A��A�v�A�  A���A���A�{A��hA���A��;A� �A���A��A��/A��wA�ƨA�Q�A�Q�A�Q�A���A�hsA�I�A���A��A��
A��RA�\)A�E�A��A�~�A��wA���A�%A�JA��A��A��A� �A}�TAzv�Aw��Atn�Ar�/An$�Al  AhI�Aa|�A_�A^1'A[�AXJAU��AS�AQS�AO�AM�AK�AF��AB��A?O�A<��A<5?A;hsA:�A9��A7XA4��A3�
A3?}A2�\A2  A1oA/A.��A.1'A-VA+�A+oA*�+A*=qA)�^A(�A(r�A({A't�A&v�A%��A$A�A#�A"bNA!�A!K�A!�A �HA ZA�Ap�A �A?}A�jA-A"�A��A1A�RA��A��AA�A��A�#A�^Ap�AdZA|�AC�A��A  At�A�Av�A�Ax�A�A��A�A�A��A��AffA�A"�A�yA��Al�A&�Ar�A�7A`BA	��A�7A;dA��A�AbNAM�A�A;dAA�jA^5A1'A��A�A ��A ��A �/A Z@��y@��^@�b@��@��P@�-@��@�P@�K�@��@��@�-@�`B@��@�A�@���@�w@ꗍ@��@�7@�@��@�"�@�R@噚@�G�@�%@��/@�A�@��@���@�E�@�&�@�  @�;d@���@އ+@�5?@�M�@�$�@���@�ff@��@�
=@�M�@݉7@���@܃@��;@���@�n�@�E�@�J@�G�@���@ؼj@�Z@�V@���@�j@��@�1'@���@�Q�@ו�@�
=@�V@�M�@�~�@�M�@�J@և+@թ�@Դ9@�1'@���@�S�@�"�@���@�^5@�-@�=q@��@�G�@���@�Z@���@�n�@��@̓u@�Q�@��m@˅@���@�E�@��@�@���@��#@�7L@Ȭ@ț�@�bN@���@�dZ@�@�~�@��@Ĵ9@Õ�@��@°!@��@��-@���@��h@�X@��j@���@�Z@� �@��
@�l�@��@��@���@���@���@��@��`@��9@��u@�1'@��;@�|�@�"�@���@���@�5?@��@���@��-@�`B@�&�@�r�@��F@���@�ff@�J@���@�O�@���@���@��9@���@��u@��@�r�@�Z@�A�@� �@��m@�+@���@�E�@���@��^@��^@��h@��@��j@�j@�9X@��@�;d@�o@��@�n�@�J@��7@�&�@���@���@��9@���@�j@�9X@�  @��w@��P@�|�@�t�@�dZ@�dZ@�\)@�S�@�C�@�+@�"�@���@�{@���@�`B@��@�j@�  @�|�@�33@��@���@�~�@�E�@��7@��u@��@���@�+@��y@��!@�@�?}@��@��u@�I�@�A�@�  @�ƨ@�l�@�l�@�l�@�;d@��@�
=@��\@���@�`B@��@��u@��@�(�@���@���@�l�@�o@���@�~�@�@���@��h@��j@��u@�I�@�1'@� �@�1@���@�C�@�o@�ȴ@�E�@�@���@��7@�x�@�hs@�G�@���@��`@���@��j@��G�O�@�
=@���@��@� �@t��@m�h@e�@^V@V�@N�y@F$�@=�@8��@17L@+@&E�@ 1'@ƨ@@~�@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ǮB	ÖB	��B	��B	�wB	�qB	�jB	�dB	�XB	�9B	�'B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B
uB
1'B
5?B
O�B
hsB
��B
�jB
�/B
��BJBoB�B�B6FBXB`BBhsBp�Bu�B�%B�PB�oB��B�!B�FB��B�/B�ZB��BB
=BbB	7B  B��B��B�B��B�9Bm�B�BB
�B
��B
��B
��B
�PB
�B
q�B
u�B
ffB
^5B
YB
<jB
�B	��B	�B	�B	ƨB	�RB	�B	��B	�DB	n�B	e`B	YB	;dB	.B	0!B	(�B	�B	�B	VB		7B	1B	+B	B	B	PB	�B	oB	{B	�B	�B	�B	-B	:^B	B�B	E�B	G�B	I�B	M�B	YB	[#B	]/B	`BB	jB	o�B	u�B	x�B	z�B	|�B	}�B	~�B	�B	�B	�B	�B	�B	�B	�B	~�B	|�B	|�B	z�B	x�B	u�B	p�B	n�B	m�B	n�B	m�B	k�B	ffB	]/B	ffB	r�B	|�B	y�B	�B	�1B	�7B	�=B	�DB	�PB	�hB	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�hB	��B	�oB	�VB	�+B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�bB	�B	�B	�B	�B	�B	�B	�B	�B	�7B	�JB	�JB	�PB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�9B	�FB	�RB	�dB	�qB	��B	ŢB	��B	��B	��B	��B	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�/B	�)B	�)B	�/B	�BB	�sB	�mB	�fB	�`B	�TB	�`B	�B	�B	�B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
+B
+B
+B
1B
1B
1B
1B
	7B
	7B
	7B
	7B
	7B
	7B
	7B

=B

=B

=B

=B

=B

=B

=B

=B
DB
DB
DB
DB
DB
DB
DB
DB
JB
JB
DB
JB
PB
PB
VB
VB
\B
\B
VB
PB
PB
PB
PB
JB
DB
	7B
1B
+B
+B
%B
%B
+B
1B
1B
1B
1B
1B
	7B

=B
JB
DB
JB
JB
JB
JB
JB
PB
VB
bB
hB
oB
oB
oB
oB
oB
oB
oB
oB
oB
oB
oB
uB
uB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
#�B
,B
7LB
=qB
C�B
G�B
L�B
Q�B
W
B
ZB
`BB
aHB
e`B
k�B
o�B
r�B
v�B
z�B
}�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B	��B	ѾB	��B	��B	��B	��B	��B	��B	еB	ͥB	ʒB	ǀB	�gB	�TB	�SB	�GB	�CB	�;B	�5B	�(B	�B	��B	��B	��B	�eB	�^B	�cB	�jB	�wB	�}B	��B	��B	��B	��B	��B	��B	��B	�^B	��B	ͣB
CB
0�B
5B
O�B
hEB
�WB
�7B
�B
��BB>BOBuB6BW�B`Bh>BpqBu�B��B�!B�=B��B��B�B��B��B�"B��B�B
B,B	B��B��B��B�QBѸB�Bm[BhB �B
�^B
�OB
��B
�bB
�B
��B
qnB
u�B
f/B
]�B
X�B
</B
KB	��B	�NB	��B	�lB	�B	��B	�^B	�B	n\B	e!B	X�B	;"B	-�B	/�B	(�B	rB	JB	B	�B	�B	�B	�B	�B	B	FB	,B	9B	JB	jB	{B	,�B	:B	BJB	E\B	GkB	IvB	M�B	X�B	Z�B	\�B	_�B	j:B	oXB	u~B	x�B	z�B	|�B	}�B	~�B	��B	��B	��B	��B	��B	��B	��B	~�B	|�B	|�B	z�B	x�B	u}B	p_B	nQB	mMB	nPB	mJB	k=B	fB	\�B	fB	riB	|�B	y�B	��B	��B	��B	��B	��B	�	B	� B	�;B	�dB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�oB	�kB	�eB	�dB	��B	��B	�uB	��B	�KB	�B	�BB	�%B	�B	��B	�?B	�>B	�JB	�QB	�WB	�cB	�qB	�gB	�UB	�WB	�gB	��B	�{B	��B	�{B	�]B	�@B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�$B	�-B	�<B	�OB	�VB	�]B	�`B	�nB	�tB	�rB	�rB	�sB	�vB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�%B	�@B	�VB	�|B	͇B	́B	�zB	�oB	́B	͈B	�uB	́B	ЙB	ѠB	ЛB	ЙB	ҩB	շB	��B	��B	��B	��B	��B	�'B	� B	�B	�B	�B	�B	�3B	�EB	�^B	��B	��B	�oB	�eB	�^B	�WB	�XB	�YB	�WB	�iB	��B	��B	��B	��B	�yB	�\B	�`B	�jB	�hB	�iB	�kB	�dB	�aB	�[B	�\B	�[B	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B
	�B
	�B
	�B
	�B
	�B
	�B
	�B

�B

�B

�B

�B

�B

�B

�B

�B
�B
�B

�B
�B
B
B
	B
B
B
B
B
B
B
B
B
�B

�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B
�B

�B
�B
�B
�B
�B
�B
B

B
B
B
B
 B
!B
!B
#B
"B
"B
"B
"B
 B
B
(B
%B
&B
-B
-B
-B
3B
4B
2B
3B
5B
7B
8B
>B
>B
?B
BB
FB
DB
KB
KB
MG�O�B
TB
#�B
+�B
6�B
=#B
CIB
G`B
L�B
Q�B
V�B
Y�B
_�B
`�B
eB
k7B
oOB
raB
v|B
z�B
}�B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.6 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             201904051008052019040510080520190405100805  AO  ARCAADJP                                                                    20170729170248    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20170729170248  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20170729170248  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190405100805  IP                  G�O�G�O�G�O�                