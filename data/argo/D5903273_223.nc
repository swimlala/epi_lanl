CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  }   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2019-02-19T18:17:36Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         ZPRIMARY | https://orcid.org/0000-0001-7324-3159 | Matthew Alkire, University of Washington        F   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    =    FORMAT_VERSION                 	long_name         File format version    
_FillValue                    =0   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    =4   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    =8   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    =H   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    =X   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    =h   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  �  =�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  �  >@   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  �  ?    CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        ?�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    ?�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    ?�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                  `  ?�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    @8   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    @D   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                  `  @H   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                  `  @�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                  `  A   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    Ah   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           At   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    A�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            A�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           A�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           A�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    A�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    A�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    A�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        D�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    E    PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    E   PROFILE_CNDC_QC                	long_name         #Global quality flag of CNDC profile    conventions       Argo reference table 2a    
_FillValue                    E   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    E   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        )�  E   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
x  n�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     )�  yd   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
x  �@   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     )�  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     )�  ה   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
x p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     )� �   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
x 5�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     )� @<   CNDC         
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     )� j   CNDC_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
x ��   CNDC_ADJUSTED            
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     )� �l   CNDC_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
x �H   CNDC_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         mhos/m     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     )� ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     )� ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
x &x   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     )� 0�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
x Z�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     )� eD   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � �    SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  � ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                  , �x   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                  0 ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                  0 �Argo profile    3.1 1.2 19500101000000  20190219181736  20200831165028  5903273 5903273 5903273 US ARGO PROJECT                                                 US ARGO PROJECT                                                 US ARGO PROJECT                                                 STEPHEN RISER                                                   STEPHEN RISER                                                   STEPHEN RISER                                                   PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL               �   �   �AAA AOAOAO  3334                            3334                            3334                            2C  2B  2C  DAD APEX                            APEX                            APEX                            4917                            4917                            4917                            041310                          041310                          041310                          846 846 846 @ְfflp@ְfflp@ְfflp111 @ְ�}.@ְ�}.@ְ�}.@6�33333@6�33333@6�33333�b�n��O��b�n��O��b�n��O�111 GPS     GPS     GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                 Near-surface sampling: discrete, unpumped [auxiliary STS]                                                                                                                                                                                                       Secondary sampling: discrete [high frequency 0.1 dbar data, for cross-calibration with the STS]                                                                                                                                                                    �   �   �ADA BDA  DA BDA @,��@�  @�  A   A   AA��A`  A�  A�  A�  A�  A�  A�  A���A�  B   B  B  B  B   B(  B0ffB8  B@  BH  BP  BX  B`  Bh  Bp��Bw��B�  B�  B�  B���B�  B�  B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDy� D�� D�IHD���D��)D�
=D�H D��D��D��D�D{D�x�D�D��D�;�D�y�D�{D���D�(RD�eqD�=�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O��L�;����������;L�;��������L�;L�;����L�;L�;��������L�;��������������;��������L�;L�;��������������������������;L�;��������L�ͽ��;��������L�;��������������������������ͽ��;��������������������L�;��������������������������������L�;��������L�ͽ��;������������������������������������������ͽ��;��������L�;L�;������������������������L�;�������������������    �L�;����L�;����������������L�ͽ��;������������������������������������L�;������������L��    ���������L�;��������L�;L�;������������L�;��������L�;������������������;����L�;����������������L�;L�;����������������������������������������L�;����������������������������������;L�;����L�;����L�;��������������ͽ��;����L�;����������������������������������������������ͽ��;��������L�;L�;����L�;��������������������L�ͽ��ͽ��;L�;��������L�;����������ͽ��;L�;������������L�;L�;L�;����L�;L�;L�;L�;������������L�ͽ���    ���;����L�;L�ͽ��;L�ͽ��;��������������;L�;L�;��������L�;L�;L�;L�ͽ��;����L�ͽ��;L�;L�ͽ��ͽ��ͽ��;������ͽ���    �L�;����L�ͽ��;L�ͽ��ͽ��ͽ��ͽ���    =��ͽ��ͽ��;����L�ͽ��ͽ���    �L�ͽ��ͽ��ͽ��;L�ͽ��ͽ��ͽ���    �����L��    ���ͽ��ͽ��ͽ��ͽ���    �L�ͽ��ͽ��ͽ��ͽ��ͽ��;L�ͽ��;L�ͽ��ͽ��;L�ͽ��ͽ��ͽ���=���    �L�ͽ��ͽ��ͽ��ͽ��ͽ��ͽ��ͽ��ͽ��ͽ��ͽ��;L�;L�ͽ��ͽ��ͽ��ͽ��ͽ��ͽ��ͽ��ͽ��ͽ���=��ͽ��ͽ��ͽ��ͽ��ͽ��ͽ��ͽ��ͽ��;L�ͽ���    ���ͽ��ͽ��ͽ��ͽ���    ���ͽ���    ���ͽ���        ���ͽ���                        ����    ����    �L�ͽ��ͽ��ͽ��ͽ��ͽ��ͽ��ͽ��ͽ��;L�ͽ��;L��    ���ͽ���    ���;L��    ���;L��    =��ͽ��ͽ���        ����    ���ͽ��ͽ��ͽ��ͽ���    ���ͽ���        ���;L�ͽ��ͽ���=��ͽ���        =���        ���;L�;L�ͽ��ͽ��ͽ��;L�ͽ���    ���;L�ͽ��;L�ͽ��;L�ͽ��ͽ��ͽ���        ����    =���=���    ���ͽ��;L�ͽ���    ���;L�ͽ��ͽ��ͽ��ͽ��ͽ���    ���ͽ���=���    ���ͽ��;L�;L��                ���ͽ��ͽ��ͽ���        ���ͽ��ͽ���        ���;L�;L�ͽ��ͽ��ͽ���    ���ͽ��ͽ��ͽ��ͽ���        �L�ͽ��ͽ���    ���ͽ��ͽ��;L�ͽ��ͽ��ͽ���=��ͽ��ͽ��ͽ��ͽ��ͽ���    ����    ���ͽ��ͽ���    ���ͽ���    ����            =���    ���ͽ��;L�;L�ͽ���    ����    =���=���>L��>���>���>���?   ?   ?��?��?333?L��?L��?�  ?���?���?���?�ff?���?�33?�  ?���?���?ٙ�?�ff?�ff?�33@   @ff@ff@��@��@��@   @&ff@333@9��@9��@Fff@L��@S33@`  @fff@l��@y��@�  @�ff@���@�  @�33@���@���@�  @�ff@���@�  @�33@���@���@�33@�ff@���@�  @�ff@ٙ�@���@�33@陚@���@�  @�ff@���A   A��A33AffA  A	��A��AffA  A33A��AffA��A33A��A   A!��A#33A&ffA(  A)��A+33A.ffA0  A1��A333A4��A8  A9��A;33A<��A>ffA@  AA��AC33AFffAH  AI��AK33AL��ANffAP  AQ��AS33AT��AVffAX  AY��A[33A\��A^ffA`  Aa��Ac33Ad��AfffAh  Ai��Al��AnffAp  Aq��As33AvffAx  Ay��A{33A~ffA�  A���A�ffA�33A�  A���A�ffA�33A���A���A�ffA�33A���A���A�33A�  A���A���A�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A���A�ffA�33A���A���A�ffA�33A�  A���A�ffA�33A�  A���A���A�ffA�33A���A���A�ffA�33A���A���A�ffA�33A���A���A�ffA�33A���A���A�ffA�  A���Ař�A�33A�  A���Aə�A�33A�  A���A͙�A�33A�  A���Aљ�A�33A�  A���Aՙ�A�33A�  A���Aٙ�A�ffA�  Dq33Dq9�DqFfDqL�DqS3DqY�DqffDql�Dqs3Dqy�Dq�fDq��Dq�3Dq� Dq�fDq��Dq�3Dq� Dq�fDq��DqٚDq� Dq�fDq��Dq��Dr  DrfDr�Dr�Dr  Dr&fDr,�Dr9�Dr@ DrFfDrS3DrY�Dr` DrffDrs3Dry�Dr� Dr��Dr�3Dr��Dr� Dr��Dr�3Dr��Dr� Dr��Dr�3DrٚDr� Dr��Dr�3Dr��DsfDs�Ds3Ds  Ds&fDs,�Ds9�Ds@ DsFfDsL�DsY�Ds` DsffDss3Dsy�Ds� Ds�fDs�3Ds��Ds� Ds��Ds�3Ds��Ds�fDs��Ds�3DsٚDs�fDs��Ds�3Ds��DtfDt�Dt3Dt  Dt&fDt,�Dt9�Dt@ DtFfDtS3DtY�Dt` DtffDts3Dty�Dt� Dt��Dt�3Dt��Dt� Dt��Dt�3Dt��Dt�fDt��Dt�3Dt� Dt�fDt��Dt�3Du  Duf@   @&ff@333@9��@9��@Fff@L��@S33@`  @fff@l��@y��@�  @�ff@���@�  @�33@���@���@�  @�ff@���@�  @�33@���@���@�33@�ff@���@�  @�ff@ٙ�@���@�33@陚@���@�  @�ff@���A   A��A33AffA  A	��A��AffA  A33A��AffA��A33A��A   A!��A#33A&ffA(  A)��A+33A.ffA0  A1��A333A4��A8  A9��A;33A<��A>ffA@  AA��AC33AFffAH  AI��AK33AL��ANffAP  AQ��AS33AT��AVffAX  AY��A[33A\��A^ffA`  Aa��Ac33Ad��AfffAh  Ai��Al��AnffAp  Aq��As33AvffAx  Ay��A{33A~ffA�  A���A�ffA�33A�  A���A�ffA�33A���A���A�ffA�33A���A���A�33A�  A���A���A�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A���A�ffA�33A���A���A�ffA�33A�  A���A�ffA�33A�  A���A���A�ffA�33A���A���A�ffA�33A���A���A�ffA�33A���A���A�ffA�33A���A���A�ffA�  A���Ař�A�33A�  A���Aə�A�33A�  A���A͙�A�33A�  A���Aљ�A�33A�  A���Aՙ�A�33A�  A���Aٙ�A�ffA�  Dq33Dq9�DqFfDqL�DqS3DqY�DqffDql�Dqs3Dqy�Dq�fDq��Dq�3Dq� Dq�fDq��Dq�3Dq� Dq�fDq��DqٚDq� Dq�fDq��Dq��Dr  DrfDr�Dr�Dr  Dr&fDr,�Dr9�Dr@ DrFfDrS3DrY�Dr` DrffDrs3Dry�Dr� Dr��Dr�3Dr��Dr� Dr��Dr�3Dr��Dr� Dr��Dr�3DrٚDr� Dr��Dr�3Dr��DsfDs�Ds3Ds  Ds&fDs,�Ds9�Ds@ DsFfDsL�DsY�Ds` DsffDss3Dsy�Ds� Ds�fDs�3Ds��Ds� Ds��Ds�3Ds��Ds�fDs��Ds�3DsٚDs�fDs��Ds�3Ds��DtfDt�Dt3Dt  Dt&fDt,�Dt9�Dt@ DtFfDtS3DtY�Dt` DtffDts3Dty�Dt� Dt��Dt�3Dt��Dt� Dt��Dt�3Dt��Dt�fDt��Dt�3Dt� Dt�fDt��Dt�3Du  DufG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111199999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 @3�
@��@ÅAA!AC\)AaA��HA��HA��HA��HA��HA��HA�A��HB p�Bp�Bp�Bp�B p�B(p�B0�
B8p�B@p�BHp�BPp�BXp�B`p�Bhp�Bq=qBx
>B�8RB�8RB�8RB�B�8RB�8RB�k�B�B�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RC )C)C)C)C)C
)C)C)C)C)C)C)C)C)C)C)C )C")C$)C&)C()C*)C,)C.)C0)C2)C4)C6)C8)C:)C<)C>)C@)CB)CD)CF)CH)CJ)CL)CN)CP)CR)CT)CV)CX)CZ)C\)C^)C`)Cb)Cd)Cf)Ch)Cj)Cl)Cn)Cp)Cr)Ct)Cv)Cx)Cz)C|)C~)C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�D 
D �
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D	
D	�
D

D
�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D 
D �
D!
D!�
D"
D"�
D#
D#�
D$
D$�
D%
D%�
D&
D&�
D'
D'�
D(
D(�
D)
D)�
D*
D*�
D+
D+�
D,
D,�
D-
D-�
D.
D.�
D/
D/�
D0
D0�
D1
D1�
D2
D2�
D3
D3�
D4
D4�
D5
D5�
D6
D6�
D7
D7�
D8
D8�
D9
D9�
D:
D:�
D;
D;�
D<
D<�
D=
D=�
D>
D>�
D?
D?�
D@
D@�
DA
DA�
DB
DB�
DC
DC�
DD
DD�
DE
DE�
DF
DF�
DG
DG�
DH
DH�
DI
DI�
DJ
DJ�
DK
DK�
DL
DL�
DM
DM�
DN
DN�
DO
DO�
DP
DP�
DQ
DQ�
DR
DR�
DS
DS�
DT
DT�
DU
DU�
DV
DV�
DW
DW�
DX
DX�
DY
DY�
DZ
DZ�
D[
D[�
D\
D\�
D]
D]�
D^
D^�
D_
D_�
D`
D`�
Da
Da�
Db
Db�
Dc
Dc�
Dd
Dd�
De
De�
Df
Df�
Dg
Dg�
Dh
Dh�
Di
Di�
Dj
Dj�
Dk
Dk�
Dl
Dl�
Dm
Dm�
Dn
Dn�
Do
Do�
Dp
Dp�
Dq
Dq�
Dr
Dr�
Ds
Ds�
Dt
Dt�
Dt�pDy�
D���D�L�D��3D�߮D��D�K�D���D�љD�)D�H D�|{D��D�
D�?
D�}D� D��3D�+�D�h�D�AHG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O���Q�B�]�B�]<#�	��Q�B�]�B�]��Q콸Q�B�]��Q콸Q�B�]�B�]��Q�B�]�B�]�B�]<#�	�B�]�B�]��Q콸Q�B�]�B�]�B�]�B�]�B�]�B�]<#�	��Q�B�]�B�]��Q�<#�	�B�]�B�]��Q�B�]�B�]�B�]�B�]�B�]�B�]<#�	<#�	�B�]�B�]�B�]�B�]�B�]��Q�B�]�B�]�B�]�B�]�B�]�B�]�B�]�B�]��Q�B�]�B�]��Q�<#�	�B�]�B�]�B�]�B�]�B�]�B�]�B�]�B�]�B�]�B�]<#�	<#�	�B�]�B�]��Q콸Q�B�]�B�]�B�]�B�]�B�]�B�]��Q�B�]�B�]�B�]�B�]�B�]=�G���Q�B�]��Q�B�]�B�]�B�]�B�]��Q�<#�	�B�]�B�]�B�]�B�]�B�]�B�]�B�]�B�]�B�]��Q�B�]�B�]�B�]��Q�=�G��B�]�B�]��Q�B�]�B�]��Q콸Q�B�]�B�]�B�]��Q�B�]�B�]��Q�B�]�B�]�B�]�B�]<#�	�B�]��Q�B�]�B�]�B�]�B�]��Q콸Q�B�]�B�]�B�]�B�]�B�]�B�]�B�]�B�]�B�]�B�]��Q�B�]�B�]�B�]�B�]�B�]�B�]�B�]�B�]<#�	��Q�B�]��Q�B�]��Q�B�]�B�]�B�]<#�	<#�	�B�]��Q�B�]�B�]�B�]�B�]�B�]�B�]�B�]�B�]�B�]�B�]�B�]<#�	<#�	�B�]�B�]��Q콸Q�B�]��Q�B�]�B�]�B�]�B�]�B�]��Q�<#�	<#�	��Q�B�]�B�]��Q�B�]�B�]<#�	<#�	��Q�B�]�B�]�B�]��Q콸Q콸Q�B�]��Q콸Q콸Q콸Q�B�]�B�]�B�]��Q�<#�	=�G�<#�	�B�]��Q콸Q�<#�	��Q�<#�	�B�]�B�]�B�]<#�	��Q콸Q�B�]�B�]��Q콸Q콸Q콸Q�<#�	�B�]��Q�<#�	��Q콸Q�<#�	<#�	<#�	�B�]<#�	<#�	=�G���Q�B�]��Q�<#�	��Q�<#�	<#�	<#�	<#�	=�G�>W
><#�	<#�	�B�]��Q�<#�	<#�	=�G���Q�<#�	<#�	<#�	��Q�<#�	<#�	<#�	=�G��B�]��Q�=�G�<#�	<#�	<#�	<#�	<#�	=�G���Q�<#�	<#�	<#�	<#�	<#�	��Q�<#�	��Q�<#�	<#�	��Q�<#�	<#�	<#�	>W
>=�G���Q�<#�	<#�	<#�	<#�	<#�	<#�	<#�	<#�	<#�	<#�	��Q콸Q�<#�	<#�	<#�	<#�	<#�	<#�	<#�	<#�	<#�	>W
><#�	<#�	<#�	<#�	<#�	<#�	<#�	<#�	��Q�<#�	=�G�<#�	<#�	<#�	<#�	<#�	=�G�<#�	<#�	=�G�<#�	<#�	=�G�=�G�<#�	<#�	=�G�=�G�=�G�=�G�=�G�=�G�<#�	=�G�<#�	=�G���Q�<#�	<#�	<#�	<#�	<#�	<#�	<#�	<#�	��Q�<#�	��Q�=�G�<#�	<#�	=�G�<#�	��Q�=�G�<#�	��Q�=�G�>W
><#�	<#�	=�G�=�G�<#�	=�G�<#�	<#�	<#�	<#�	<#�	=�G�<#�	<#�	=�G�=�G�<#�	��Q�<#�	<#�	>W
><#�	=�G�=�G�>W
>=�G�=�G�<#�	��Q콸Q�<#�	<#�	<#�	��Q�<#�	=�G�<#�	��Q�<#�	��Q�<#�	��Q�<#�	<#�	<#�	=�G�=�G�<#�	=�G�>W
>>W
>=�G�<#�	<#�	��Q�<#�	=�G�<#�	��Q�<#�	<#�	<#�	<#�	<#�	=�G�<#�	<#�	>W
>=�G�<#�	<#�	��Q콸Q�=�G�=�G�=�G�=�G�<#�	<#�	<#�	<#�	=�G�=�G�<#�	<#�	<#�	=�G�=�G�<#�	��Q콸Q�<#�	<#�	<#�	=�G�<#�	<#�	<#�	<#�	<#�	=�G�=�G���Q�<#�	<#�	=�G�<#�	<#�	<#�	��Q�<#�	<#�	<#�	>W
><#�	<#�	<#�	<#�	<#�	=�G�<#�	=�G�<#�	<#�	<#�	=�G�<#�	<#�	=�G�<#�	=�G�=�G�=�G�>W
>=�G�<#�	<#�	��Q콸Q�<#�	=�G�<#�	=�G�>W
>>W
>>��R>��>��?�\?(�?(�?5?5?O\)?h��?h��?�{?��H?��H?��H?�z�?��?�G�?�{?��H?��H?�?�z�?�z�@ ��@
=@p�@p�@�
@ ��@ ��@'
=@-p�@:=p@@��@@��@Mp�@S�
@Z=p@g
=@mp�@s�
@�Q�@��@��@��@��@��R@��@�Q�@��@��@��@��@��R@��@�Q�@ƸR@��@�Q�@Ӆ@��@��@�Q�@�R@��@�Q�@�@��@��AA\)A��A(�A	A\)A�\A(�AA��A�\A(�A\)A��A�\A!A#\)A$��A((�A)A+\)A,��A0(�A1A3\)A4��A6�\A9A;\)A<��A>�\A@(�AAAC\)AD��AH(�AIAK\)AL��AN�\AP(�AQAS\)AT��AV�\AX(�AYA[\)A\��A^�\A`(�AaAc\)Ad��Af�\Ah(�AiAk\)An�\Ap(�AqAs\)At��Ax(�AyA{\)A|��A�{A��HA�z�A�G�A�{A��HA�z�A�G�A�{A��A�z�A�G�A�{A��A�z�A�{A��HA��A�z�A�{A��HA��A�G�A�{A��HA�z�A�G�A�{A��HA�z�A�G�A�{A��HA��A�G�A�{A��HA��A�z�A�G�A�{A��A�z�A�G�A�{A��HA��A�G�A�{A��HA��A�z�A�G�A�{A��A�z�A�G�A�{A��A�z�A�G�A�{A��A�z�A�G�A�{A��A�z�A�G�A��HAŮA�z�A�{A��HAɮA�z�A�{A��HAͮA�z�A�{A��HAѮA�z�A�{A��HAծA�z�A�{A��HAٮA�z�A�G�A��HDq:=Dq@�DqMpDqS�DqZ=Dq`�DqmpDqs�Dqz=Dq��Dq�pDq��Dq�=Dq�
Dq�pDq��Dq�=Dq�
Dq�pDq��Dq�Dq�
Dq�pDq��Dr �Dr
DrpDr�Dr �Dr'
Dr-pDr3�Dr@�DrG
DrMpDrZ=Dr`�Drg
DrmpDrz=Dr��Dr�
Dr��Dr�=Dr��Dr�
Dr��Dr�=Dr��Dr�
Dr��Dr�=Dr�Dr�
Dr��Dr�=Ds �DspDs�Ds=Ds'
Ds-pDs3�Ds@�DsG
DsMpDsS�Ds`�Dsg
DsmpDsz=Ds��Ds�
Ds�pDs�=Ds��Ds�
Ds��Ds�=Ds��Ds�pDs��Ds�=Ds�Ds�pDs��Ds�=Dt �DtpDt�Dt=Dt'
Dt-pDt3�Dt@�DtG
DtMpDtZ=Dt`�Dtg
DtmpDtz=Dt��Dt�
Dt��Dt�=Dt��Dt�
Dt��Dt�=Dt��Dt�pDt��Dt�=Dt�
Dt�pDt��Dt�=Du
Dup@'
=@-p�@:=p@@��@@��@Mp�@S�
@Z=p@g
=@mp�@s�
@�Q�@��@��@��@��@��R@��@�Q�@��@��@��@��@��R@��@�Q�@ƸR@��@�Q�@Ӆ@��@��@�Q�@�R@��@�Q�@�@��@��AA\)A��A(�A	A\)A�\A(�AA��A�\A(�A\)A��A�\A!A#\)A$��A((�A)A+\)A,��A0(�A1A3\)A4��A6�\A9A;\)A<��A>�\A@(�AAAC\)AD��AH(�AIAK\)AL��AN�\AP(�AQAS\)AT��AV�\AX(�AYA[\)A\��A^�\A`(�AaAc\)Ad��Af�\Ah(�AiAk\)An�\Ap(�AqAs\)At��Ax(�AyA{\)A|��A�{A��HA�z�A�G�A�{A��HA�z�A�G�A�{A��A�z�A�G�A�{A��A�z�A�{A��HA��A�z�A�{A��HA��A�G�A�{A��HA�z�A�G�A�{A��HA�z�A�G�A�{A��HA��A�G�A�{A��HA��A�z�A�G�A�{A��A�z�A�G�A�{A��HA��A�G�A�{A��HA��A�z�A�G�A�{A��A�z�A�G�A�{A��A�z�A�G�A�{A��A�z�A�G�A�{A��A�z�A�G�A��HAŮA�z�A�{A��HAɮA�z�A�{A��HAͮA�z�A�{A��HAѮA�z�A�{A��HAծA�z�A�{A��HAٮA�z�A�G�A��HDq:=Dq@�DqMpDqS�DqZ=Dq`�DqmpDqs�Dqz=Dq��Dq�pDq��Dq�=Dq�
Dq�pDq��Dq�=Dq�
Dq�pDq��Dq�Dq�
Dq�pDq��Dr �Dr
DrpDr�Dr �Dr'
Dr-pDr3�Dr@�DrG
DrMpDrZ=Dr`�Drg
DrmpDrz=Dr��Dr�
Dr��Dr�=Dr��Dr�
Dr��Dr�=Dr��Dr�
Dr��Dr�=Dr�Dr�
Dr��Dr�=Ds �DspDs�Ds=Ds'
Ds-pDs3�Ds@�DsG
DsMpDsS�Ds`�Dsg
DsmpDsz=Ds��Ds�
Ds�pDs�=Ds��Ds�
Ds��Ds�=Ds��Ds�pDs��Ds�=Ds�Ds�pDs��Ds�=Dt �DtpDt�Dt=Dt'
Dt-pDt3�Dt@�DtG
DtMpDtZ=Dt`�Dtg
DtmpDtz=Dt��Dt�
Dt��Dt�=Dt��Dt�
Dt��Dt�=Dt��Dt�pDt��Dt�=Dt�
Dt�pDt��Dt�=Du
DupG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111199999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�9XA�33A�5?A�(�A�+A�(�A�7LA��HA���A��
A���A̮A�^5A�\)A�Q�A�O�A�M�A�K�A�K�A�I�A�G�A�E�A�A�A�;dA�7LA�+A�%A��`A�I�AʅA���A�1'AǅA�M�A�t�A�=qA��PA�$�A��A��`A��yA�ƨA�bNA�%A�n�A�$�A� �A��;A�oA�JA���A��TA�1A�I�A�A�ffA��9A�9XA�bA��A��!A�S�A��;A�1A��RA�+A�XA��7A���A�{A��FA�  A�K�A�7LA��uA��A���A��A��/A��A�A�A��A�l�A��A��A��-A�"�A��A���A�A��A��;A�z�A��A�=qA�"�A�bA��yA�`BA��7A��A���A���A�dZA�O�A��A��7A�7LA��A�v�A�=qA�33A��+A�1A�t�A��A��A}hsA|VAy��AuK�Ar�RAo�An-Al1'AjVAh^5Af~�Ad��Aa|�A^A\�DAZ�DAWK�AU;dAS�AP��AO�PAM7LAJ�AI��AI33AH1'AG\)AFbNAEAB�9A?�A>�9A=%A<�A;x�A:��A:~�A:A9x�A8�\A7�7A6ȴA5��A5?}A4ȴA2jA1VA0ffA0=qA.�/A-�^A,=qA+7LA*9XA)�A(�`A(1A'�A%��A$5?A"��A"-A!;dA 5?AC�AbNAbA��A��A��A33A�+A�AG�A�!A�AVA�+A�FA�AȴA1AAM�AVAA�A�wA;dA=qA�A
ffA	�^A�A=qA|�Az�AoAdZA��AE�AƨA&�A j@��@��y@���@���@���@��;@�33@�$�@��9@�9X@�|�@�@�G�@��m@�-@�7@�j@�V@�u@�33@��@��@�n�@�J@�@�z�@�"�@���@���@�1@�"�@�ȴ@�V@�O�@� �@���@���@ش9@� �@ו�@��H@���@�I�@��H@�j@�=q@̋D@�+@�@�%@�ƨ@��@���@�Ĝ@ÍP@��#@���@�^5@��@���@��@���@�v�@�G�@�%@�G�@�(�@��R@�5?@���@���@�l�@�@�x�@��@�A�@���@�;d@���@���@�M�@��@��7@�p�@���@�I�@��@���@�r�@�Z@���@��m@�+@���@�Ĝ@���@���@���@�K�@��@��y@�|�@��@��!@��\@�V@�M�@�@��@��^@��@�?}@��@��9@�Z@��P@��@�`B@���@�^5@�ff@�=q@�5?@��@��@�J@��@�p�@�%@���@��@��u@�9X@���@��w@���@�dZ@�;d@��y@���@��\@��@���@��T@�=q@��#@�n�@�~�@��@���@��^@��-@���@�&�@�V@��@�(�@���@���@���@��m@���@�|�@�C�@�o@��@���@���@���@��@���@���@�E�@���@�?}@��`@�z�@�Q�@�ƨ@��@��@��H@���@��@���@�J@�`B@��/@�j@�j@��@���@�  @���@��
@��F@�;d@�\)@�dZ@�\)@�
=@�C�@�t�@�+@��y@���@���@���@��#@���@�$�@�J@�@�`B@�%@��`@��@�b@�1@��;@�t�@�\)@�"�@��@���@�~�@�V@�@�X@��@���@���@��@�A�@���@�l�@�33@��@��@�@��!@�ff@�5?@���@�hs@�/@���@��`@��9@�j@�(�@�1@��;@��w@���@�t�@�C�@��@��R@�n�@�-@��@���@��7@�x�@�`B@�7L@��@��@�j@�	@}/@up�@k��@c@[�:@Rs�@I��@C6z@=T�@7ƨ@1A @+�@&��@!x�@��@h
@�2@��@�@F�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A̍PAɴ9A���A��A�n�AɅA�E�A�A��/A���A�
=A�
=A���Ạ�A� �A��A��A̋DA̴9A�{A�r�A��A�`BA�-A�^5A��A��A�dZA��A�A��TA��/A�jA�oA�Q�AǸRA�$�A�E�A\A¸RA� �A�A�A���A��#A�VAĶFA���A�{A���A�A�5?Aĥ�A��yA��
A�A�S�A�33A�t�A���AˮA���A�1'A��/A�VA��A�^5Aĺ^A�"�AŴ9A��HA�+A�G�A�K�A�\)A�hsA�bA�|�A�bNA��A��A�^5A�bNA���A�~�Aǧ�A�ffA�ȴA�VA��A�$�A�?}A�%A���A�bA�&�A�XAˍPA�~�A��Aʉ7A�oA�{A̶FA��A��TA��9A�  A�v�A��7A���A�ffA���A�=qA×�A�t�A�bNA�{A��Aǉ7A�1A�  A�|�A�&�A�bA�^5AœuAȩ�A�  A�z�A�;dA��`A˶FA��A��mA�(�A�v�A�bNA�
=Að!A�VA���A�
=Aʏ\A�
=A�9XA̛�A��A�bA���A�ZA�O�A�VA���A��/A�S�A��yA�A�A�E�A�M�A��#Aº^A��
A�JA��A�bA�n�A�  A�=qA�ZA��A�O�A�A�;dA�%A�  A��A���A�1'A�E�A�oA�-A�{A�I�A���A�A�XA�ĜA�VA�{A�A�VA���A���AǼjA˛�A�dZAŃA���A��/A�AɶFA�A���A���A�VAǾwA˓uA��A�^5A���A���A���A˟�AƶFA�K�A�(�A�A�XA�ZA�1A̶FA�  A�1A�l�A�n�A�+A�
=A̛�A�bA�bA���A�C�A�bA��`A�bA�bA���A�M�Aɉ7A�bA�bA�JA�1A�ĜA�hsA�VA̓uA�VA�JA�ĜA�VA�VA�oA�oA�VA�{A�VA̾wA��A�oA�bA�oA�A�
=A�1A�VA�bA�oA�bA��A�JA�oA�{A�bA�
=A̬A�bA�bA�JA�VA�bA�VA�VA�%A�bA�bA�bA�VA�
=A�%A�bA�bA�bA̺^A�VA�VA�VA���A�bA�bA�VA�bA�VA�JA�VA�VA�bA�VA�JA�VA�JA�VA�VA�VA�bA�1A�JA�VA�VA�JA�VA�JA�A�
=A�
=A�bA�1A�bA�{A�bA�{A�JA�{A�oA�{A�oA�{A�bA�VA�bA�bA�JA�bA�oA�oA�VA��A�oA�bA�VA�VA�oA�bA�oA�bA�bA�{A�{A�{A�oA��A�bA�JA��A��A��A��A�{A��A��A�{A�oA��A��A��A�{A�{A��A�{A��A��A�oA�oA�{A��A�oA��A��A�{A��A�{A��A��A�oA�1A��A��A��A��A��A��A��A��A��A�{A�oA�{A��TA��A��A��A�VA�oA�;dA�{A�{A�{A�bA�{A��A��A��A��A��A��A��A�{A�oA�{A�
=A�oA�{A�{A�bA�JA�bA���A�bA�oA��A�{A��A��A��A��A�oA��A��A�{A��A��A��A��A��A��A��A�VA�{A��A��A��A��A��A��A��A��A��A�oA�{A�ffA��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A�{A�bA�VA�1A��A�oA��A�oA�{A�{A�oA�bA��A��A�oA�{A��A��A��A��A�{A�{A��A�bA� �A��A��A��A�"�A��A� �A�"�A��A�$�A��A��A��A�"�A�{A��A� �A��A�$�A�+A�&�A�"�A�&�A�"�A�"�A�"�A�$�A�+A�+A�+A�+A�1'A�+A�$�A� �A� �A�-A�$�A� �A�+A�1'A�/A�5?A�33A�+A�33A�1'A�/A�/A�1'A�33A�5?A�7LA�33A�33A�33A�33A�33A�33A�1'A�1'A�33A�5?A�9XA�7LA�7LA�9XA�9XA�7LA�5?A�5?A�1'A�7LA�7LA�5?A�/A�+A�+A�-A�-A�1'A�33A�9XA�7LA�5?A�1'A�5?A�33A�5?A�33A�33A�33A�5?A�5?A�1'A�33A�1'A�33A�-A�-A�-A�(�A�+A�+A�$�A�"�A�$�A� �A�(�A�1'A�-A�/A�1'A�1'A�1'A�7LA�/A�&�A�-A�1'A�33A�-A��A��A�"�A� �A�$�A��A�"�A� �A��A��A��A� �A�"�A� �A��A�-A�1'A�33A�/A�1'A�=qA�=qA�;dA�;dA�=qA�;dA�;dA�9XA�5?A�7LA�=qA�;dA�;dA�9XA�-A�5?A�5?A�;dA�5?A�=qA�;dA��mA��TA��`A��`A��A��A��yA��HA��TA��TA��`A��A��yA��TA���A��
A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��
A��A��
A��
A��
A��A��A��
A��A��
A��
A��
A��
A���A��
A��
A���A���A���A���A���A���A�ȴA�ȴA̾wA̶FA̾wA���A���A���A���A��
A���A���A���A���A���A���A�ȴA�ĜA���A���A�A���A̴9Ạ�A�z�A�S�A�VA�S�A�S�A�ZA�^5A�\)A�^5A�\)A�`BA�dZA�hsA�bNA�^5A�`BA�dZA�dZA�bNA�dZA�ffA�dZA�bNA�dZ@�@���@�@��^@��^@��^@��^@���@���@���@���@��h@���@���@���@��h@��h@��h@��h@��h@��h@��7@��7@��@��7@��7@��7@��@��@��@��@��@�x�@��@��@�x�@��@�x�@�x�@�x�@�x�@�x�@�x�@�x�@�x�@�x�@�p�@�p�@�hs@�hs@�hs@�hs@�hs@�hs@�hs@�hs@�hs@�`B@�`B@�`B@�X@�X@�O�@�G�@�G�@�G�@�G�@�G�@�?}@�?}@�7L@�7L@�7L@�/@�/@�/@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@�V@��@��@�%@��`@���@���@��@���@���@���@���@���@���@��u@��u@��u@��D@�z�@�j@�r�@�z�@�r�@�r�@�I�@�A�@�Z@�9XA�5?A�=qA�9XA�9XA�7LA�7LA�5?A�5?A�5?A�9XA�7LA�33A�-A�-A�+A�+A�(�A�1'A�9XA�9XA�7LA�33A�33A�5?A�5?A�5?A�5?A�33A�33A�5?A�33A�33A�33A�1'A�(�A�1'A�-A�+A�-A�(�A�$�A�"�A��A� �A�&�A�&�A�33A�/A�-A�/A�/A�1'A�33A�(�A�&�A�+A�+A�-A�(�A�$�A�"�A�"�A�"�A� �A��A��A�"�A� �A��A�$�A�$�A�"�A� �A�"�A�/A�1'A�33A�33A�=qA�=qA�A�A�=qA�;dA�;dA�=qA�;dA�;dA�9XA�;dA�;dA�;dA�7LA�9XA�7LA�7LA�7LA�9XA�=qA�5?A���A��TA��HA��A��yA��A��`A��#A��HA��TA��TA��TA��#A��HA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��
A��A���A��A��
A��
A��A��
A���A��
A��A��A��A��A��
A���A���A���A���A���A��
A��
A���A���A���A���A�ƨA�ƨA���A���A���A���A��
A���A��A���A���A���A���A���A�ȴA�ƨA���A�A�A���A̙�A�p�A�\)A�ffA�^5A�Q�A�Q�A�^5A�`BA�`BA�^5A�\)A�bNA�`BA�^5A�`BA�`BA�bNA�dZA�dZA�dZA�dZA�dZA�dZA�dZ@���@���@�@�@��^@��^@��^@���@���@���@���@���@���@���@���@��h@��h@��h@��h@��h@��h@��7@��7@��7@��7@��7@��7@��7@��7@��@��@��@��@��@��@��@��@��@�x�@�x�@�x�@�x�@�x�@�x�@�x�@�x�@�p�@�p�@�p�@�hs@�hs@�hs@�hs@�hs@�hs@�hs@�hs@�hs@�hs@�`B@�`B@�X@�X@�O�@�G�@�G�@�G�@�G�@�G�@�?}@�?}@�?}@�?}@�7L@�/@�/@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@�V@���@��/@���@��j@���@��@��@���@���@��u@���@���@��D@��D@��@�r�@��@�z�@��@�r�@�Q�@�Q�@�bN@�bNG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111199999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 A�9XA�33A�5?A�(�A�+A�(�A�7LA��HA���A��
A���A̮A�^5A�\)A�Q�A�O�A�M�A�K�A�K�A�I�A�G�A�E�A�A�A�;dA�7LA�+A�%A��`A�I�AʅA���A�1'AǅA�M�A�t�A�=qA��PA�$�A��A��`A��yA�ƨA�bNA�%A�n�A�$�A� �A��;A�oA�JA���A��TA�1A�I�A�A�ffA��9A�9XA�bA��A��!A�S�A��;A�1A��RA�+A�XA��7A���A�{A��FA�  A�K�A�7LA��uA��A���A��A��/A��A�A�A��A�l�A��A��A��-A�"�A��A���A�A��A��;A�z�A��A�=qA�"�A�bA��yA�`BA��7A��A���A���A�dZA�O�A��A��7A�7LA��A�v�A�=qA�33A��+A�1A�t�A��A��A}hsA|VAy��AuK�Ar�RAo�An-Al1'AjVAh^5Af~�Ad��Aa|�A^A\�DAZ�DAWK�AU;dAS�AP��AO�PAM7LAJ�AI��AI33AH1'AG\)AFbNAEAB�9A?�A>�9A=%A<�A;x�A:��A:~�A:A9x�A8�\A7�7A6ȴA5��A5?}A4ȴA2jA1VA0ffA0=qA.�/A-�^A,=qA+7LA*9XA)�A(�`A(1A'�A%��A$5?A"��A"-A!;dA 5?AC�AbNAbA��A��A��A33A�+A�AG�A�!A�AVA�+A�FA�AȴA1AAM�AVAA�A�wA;dA=qA�A
ffA	�^A�A=qA|�Az�AoAdZA��AE�AƨA&�A j@��@��y@���@���@���@��;@�33@�$�@��9@�9X@�|�@�@�G�@��m@�-@�7@�j@�V@�u@�33@��@��@�n�@�J@�@�z�@�"�@���@���@�1@�"�@�ȴ@�V@�O�@� �@���@���@ش9@� �@ו�@��H@���@�I�@��H@�j@�=q@̋D@�+@�@�%@�ƨ@��@���@�Ĝ@ÍP@��#@���@�^5@��@���@��@���@�v�@�G�@�%@�G�@�(�@��R@�5?@���@���@�l�@�@�x�@��@�A�@���@�;d@���@���@�M�@��@��7@�p�@���@�I�@��@���@�r�@�Z@���@��m@�+@���@�Ĝ@���@���@���@�K�@��@��y@�|�@��@��!@��\@�V@�M�@�@��@��^@��@�?}@��@��9@�Z@��P@��@�`B@���@�^5@�ff@�=q@�5?@��@��@�J@��@�p�@�%@���@��@��u@�9X@���@��w@���@�dZ@�;d@��y@���@��\@��@���@��T@�=q@��#@�n�@�~�@��@���@��^@��-@���@�&�@�V@��@�(�@���@���@���@��m@���@�|�@�C�@�o@��@���@���@���@��@���@���@�E�@���@�?}@��`@�z�@�Q�@�ƨ@��@��@��H@���@��@���@�J@�`B@��/@�j@�j@��@���@�  @���@��
@��F@�;d@�\)@�dZ@�\)@�
=@�C�@�t�@�+@��y@���@���@���@��#@���@�$�@�J@�@�`B@�%@��`@��@�b@�1@��;@�t�@�\)@�"�@��@���@�~�@�V@�@�X@��@���@���@��@�A�@���@�l�@�33@��@��@�@��!@�ff@�5?@���@�hs@�/@���@��`@��9@�j@�(�@�1@��;@��w@���@�t�@�C�@��@��R@�n�@�-@��@���@��7@�x�@�`B@�7L@��@��G�O�@�	@}/@up�@k��@c@[�:@Rs�@I��@C6z@=T�@7ƨ@1A @+�@&��@!x�@��@h
@�2@��@�@F�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A̍PAɴ9A���A��A�n�AɅA�E�A�A��/A���A�
=A�
=A���Ạ�A� �A��A��A̋DA̴9A�{A�r�A��A�`BA�-A�^5A��A��A�dZA��A�A��TA��/A�jA�oA�Q�AǸRA�$�A�E�A\A¸RA� �A�A�A���A��#A�VAĶFA���A�{A���A�A�5?Aĥ�A��yA��
A�A�S�A�33A�t�A���AˮA���A�1'A��/A�VA��A�^5Aĺ^A�"�AŴ9A��HA�+A�G�A�K�A�\)A�hsA�bA�|�A�bNA��A��A�^5A�bNA���A�~�Aǧ�A�ffA�ȴA�VA��A�$�A�?}A�%A���A�bA�&�A�XAˍPA�~�A��Aʉ7A�oA�{A̶FA��A��TA��9A�  A�v�A��7A���A�ffA���A�=qA×�A�t�A�bNA�{A��Aǉ7A�1A�  A�|�A�&�A�bA�^5AœuAȩ�A�  A�z�A�;dA��`A˶FA��A��mA�(�A�v�A�bNA�
=Að!A�VA���A�
=Aʏ\A�
=A�9XA̛�A��A�bA���A�ZA�O�A�VA���A��/A�S�A��yA�A�A�E�A�M�A��#Aº^A��
A�JA��A�bA�n�A�  A�=qA�ZA��A�O�A�A�;dA�%A�  A��A���A�1'A�E�A�oA�-A�{A�I�A���A�A�XA�ĜA�VA�{A�A�VA���A���AǼjA˛�A�dZAŃA���A��/A�AɶFA�A���A���A�VAǾwA˓uA��A�^5A���A���A���A˟�AƶFA�K�A�(�A�A�XA�ZA�1A̶FA�  A�1A�l�A�n�A�+A�
=A̛�A�bA�bA���A�C�A�bA��`A�bA�bA���A�M�Aɉ7A�bA�bA�JA�1A�ĜA�hsA�VA̓uA�VA�JA�ĜA�VA�VA�oA�oA�VA�{A�VA̾wA��A�oA�bA�oA�A�
=A�1A�VA�bA�oA�bA��A�JA�oA�{A�bA�
=A̬A�bA�bA�JA�VA�bA�VA�VA�%A�bA�bA�bA�VA�
=A�%A�bA�bA�bA̺^A�VA�VA�VA���A�bA�bA�VA�bA�VA�JA�VA�VA�bA�VA�JA�VA�JA�VA�VA�VA�bA�1A�JA�VA�VA�JA�VA�JA�A�
=A�
=A�bA�1A�bA�{A�bA�{A�JA�{A�oA�{A�oA�{A�bA�VA�bA�bA�JA�bA�oA�oA�VA��A�oA�bA�VA�VA�oA�bA�oA�bA�bA�{A�{A�{A�oA��A�bA�JA��A��A��A��A�{A��A��A�{A�oA��A��A��A�{A�{A��A�{A��A��A�oA�oA�{A��A�oA��A��A�{A��A�{A��A��A�oA�1A��A��A��A��A��A��A��A��A��A�{A�oA�{A��TA��A��A��A�VA�oA�;dA�{A�{A�{A�bA�{A��A��A��A��A��A��A��A�{A�oA�{A�
=A�oA�{A�{A�bA�JA�bA���A�bA�oA��A�{A��A��A��A��A�oA��A��A�{A��A��A��A��A��A��A��A�VA�{A��A��A��A��A��A��A��A��A��A�oA�{A�ffA��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A�{A�bA�VA�1A��A�oA��A�oA�{A�{A�oA�bA��A��A�oA�{A��A��A��A��A�{A�{A��A�bA� �A��A��A��A�"�A��A� �A�"�A��A�$�A��A��A��A�"�A�{A��A� �A��A�$�A�+A�&�A�"�A�&�A�"�A�"�A�"�A�$�A�+A�+A�+A�+A�1'A�+A�$�A� �A� �A�-A�$�A� �A�+A�1'A�/A�5?A�33A�+A�33A�1'A�/A�/A�1'A�33A�5?A�7LA�33A�33A�33A�33A�33A�33A�1'A�1'A�33A�5?A�9XA�5?A�=qA�9XA�9XA�7LA�7LA�5?A�5?A�5?A�9XA�7LA�33A�-A�-A�+A�+A�(�A�1'A�9XA�9XA�7LA�33A�33A�5?A�5?A�5?A�5?A�33A�33A�5?A�33A�33A�33A�1'A�(�A�1'A�-A�+A�-A�(�A�$�A�"�A��A� �A�&�A�&�A�33A�/A�-A�/A�/A�1'A�33A�(�A�&�A�+A�+A�-A�(�A�$�A�"�A�"�A�"�A� �A��A��A�"�A� �A��A�$�A�$�A�"�A� �A�"�A�/A�1'A�33A�33A�=qA�=qA�A�A�=qA�;dA�;dA�=qA�;dA�;dA�9XA�;dA�;dA�;dA�7LA�9XA�7LA�7LA�7LA�9XA�=qA�5?A���A��TA��HA��A��yA��A��`A��#A��HA��TA��TA��TA��#A��HA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��
A��A���A��A��
A��
A��A��
A���A��
A��A��A��A��A��
A���A���A���A���A���A��
A��
A���A���A���A���A�ƨA�ƨA���A���A���A���A��
A���A��A���A���A���A���A���A�ȴA�ƨA���A�A�A���A̙�A�p�A�\)A�ffA�^5A�Q�A�Q�A�^5A�`BA�`BA�^5A�\)A�bNA�`BA�^5A�`BA�`BA�bNA�dZA�dZA�dZA�dZA�dZA�dZA�dZ@���@���@�@�@��^@��^@��^@���@���@���@���@���@���@���@���@��h@��h@��h@��h@��h@��h@��7@��7@��7@��7@��7@��7@��7@��7@��@��@��@��@��@��@��@��@��@�x�@�x�@�x�@�x�@�x�@�x�@�x�@�x�@�p�@�p�@�p�@�hs@�hs@�hs@�hs@�hs@�hs@�hs@�hs@�hs@�hs@�`B@�`B@�X@�X@�O�@�G�@�G�@�G�@�G�@�G�@�?}@�?}@�?}@�?}@�7L@�/@�/@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@�V@���@��/@���@��j@���@��@��@���@���@��u@���@���@��D@��D@��@�r�@��@�z�@��@�r�@�Q�@�Q�@�bN@�bNA�5?A�=qA�9XA�9XA�7LA�7LA�5?A�5?A�5?A�9XA�7LA�33A�-A�-A�+A�+A�(�A�1'A�9XA�9XA�7LA�33A�33A�5?A�5?A�5?A�5?A�33A�33A�5?A�33A�33A�33A�1'A�(�A�1'A�-A�+A�-A�(�A�$�A�"�A��A� �A�&�A�&�A�33A�/A�-A�/A�/A�1'A�33A�(�A�&�A�+A�+A�-A�(�A�$�A�"�A�"�A�"�A� �A��A��A�"�A� �A��A�$�A�$�A�"�A� �A�"�A�/A�1'A�33A�33A�=qA�=qA�A�A�=qA�;dA�;dA�=qA�;dA�;dA�9XA�;dA�;dA�;dA�7LA�9XA�7LA�7LA�7LA�9XA�=qA�5?A���A��TA��HA��A��yA��A��`A��#A��HA��TA��TA��TA��#A��HA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��
A��A���A��A��
A��
A��A��
A���A��
A��A��A��A��A��
A���A���A���A���A���A��
A��
A���A���A���A���A�ƨA�ƨA���A���A���A���A��
A���A��A���A���A���A���A���A�ȴA�ƨA���A�A�A���A̙�A�p�A�\)A�ffA�^5A�Q�A�Q�A�^5A�`BA�`BA�^5A�\)A�bNA�`BA�^5A�`BA�`BA�bNA�dZA�dZA�dZA�dZA�dZA�dZA�dZ@���@���@�@�@��^@��^@��^@���@���@���@���@���@���@���@���@��h@��h@��h@��h@��h@��h@��7@��7@��7@��7@��7@��7@��7@��7@��@��@��@��@��@��@��@��@��@�x�@�x�@�x�@�x�@�x�@�x�@�x�@�x�@�p�@�p�@�p�@�hs@�hs@�hs@�hs@�hs@�hs@�hs@�hs@�hs@�hs@�`B@�`B@�X@�X@�O�@�G�@�G�@�G�@�G�@�G�@�?}@�?}@�?}@�?}@�7L@�/@�/@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@�V@���@��/@���@��j@���@��@��@���@���@��u@���@���@��D@��D@��@�r�@��@�z�@��@�r�@�Q�@�Q�@�bN@�bNG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111199999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�c>�6@�
�@�d�>�'|>R��@HCW@�;�?�e>��@�\h=�1Q>#@6Pr?��=Ǯ>	�r?���@�dZ>?�?%>�@�]�@(�>=��>)I�>�?z�> >�?�q@�Jw=���=�>EM�@�]%@�]%=�%�?��?ρ=���=v�=���=Ʒ�>E�@8A�@�`?��Z=�9=�-�>�ә>2�?4��@'�>=쐂>���=���=�5�=��n=�2#?r��>���@��=�?�>^�@�Y�@�`k=�/�=��>=?,�>��@f�=�A?���>�+?�@�^J@QM>K&�@�cI@�^�@�a�=鈐@dLY>Ѥ�=���>��@�@�]�=�bN=��u=��>R[@"82@�g#?(��>��@���=�>(��?�o?PsX@�Jw@�MU=OV�=Mt�=m(�=�֌=��Q=��=��e>�?�'�?���=���=���>R�s@�ek@�h�>�@�D�@�`=��:?c�:@�h4@�^t=�N>(h4@�g�@��F>��?9��@��<=�r=��> Ĝ>��T@�c^>F77?�S�=���?��Q?cD=?EN@�]�>q�@\%�>��n=��> �@�L�>/�6?X��?�ѷ=�	�>9�9@�i�>��?��=��v=މ"@(��>:�>A�@Z�b@�i?�a�?�^?��I>a��@M@?��]><�?@�@�]�@�a�>!��@�]�=��(>4/@I~>���>;��@ZG=�C�>�i@��U>?h?�"�@�i@�`>?�?�Ɇ@�k�?��>�;�@�]%> /0@R�+>à@S i>7�@�[�@�Z�@�W�=��N>[�?Xj�@�a(=�y�?GZ@�WT@�T"@��>!Ҟ@Mv`>f�@�_�@�2v?��5>oD�@��Q@�e�@�d@�j=��>�@�d�@ C�@�f'@�f'@�e>`"@�i@�J@�h�@�i@�d�=�Y�?� @�h4@�iD@�b�@i�H@Q�	?���@�f{@��@�g�@�g#@�L�?l�@�i@�j�@�j�@�i@�k�@�h�@�g�>�C�@�iY@�i@�g�@�g�@�X�@�h�@�iD@�iD@�i�@�j�@�k�@�hI@�i@�j�@�g�@�g8@X�@�hI@�hI@�hI@�hI@�i@�iY@�i@�i@�i�@�iY@�iY@�h�@�hI@��)@�i@�h�@�j@5��@�h�@�g�@�g�@�hI@�iY@�i�@�i@�i@�iY@�h�@�i@�h�@�h�@�g�@�hI@�i@�h�@�h�@�h�@�h�@�h�@�g�@�h4@�h�@�h4@�g�@�g�@�h4@�h4@�iD@�iD@�j@�j@�jU@�k@�k{@�j�@�k'@�k�@�l�@�k�@�k�@�l7@�k'@�j@�iY@�i@�j@�k'@�k�@�k�@�l�@�h�@�i�@�hI@�g8@�i�@�i�@�j@�k�@�k'@�j�@�k�@�l7@�k'@�m]@�o~@�k'@�nn@�o~@�n�@�o*@�o~@�n@�n�@�m�@�m3@�m3@�nY@�n�@�m3@�m�@�m3@�o @�m�@�m3@�m3@�k�@�l�@�l7@�m�@�j�@�n�@�nn@�m]@�l�@�l�@�l�@�k�@�j@�h�@�m]@�m]@�nn@�m]@�m�@�m]@�m]@�m]@�l�@�l�@�k�@�l�@�j�@�l7@�k�@�i�@�i�@�j?m3@�k'@�j@�m]@�l�@�m�@�n�@�o~@�o~@�n�@�p;@�nn@�l�@�j@�g�@�l�@�o�@�m�@�p�@�j@�g�@�iY@�k'@�eV@�n�@�pz@�pz@�p@�oT@�p�@�rG@�nY@�m�@�o�@�o @�n�@�o~@�o~@�p�@�qL@�p;@�o~@�m]@�n@�n�@�p�@�o~@�qL@�p�@�o�@�p�@�r@�q�@�q�@�p�@�p�@l)�@�q�@�r�@�r�@�q�@�q"@�q"@�r@�q�@�r\@�r@�p�@�p�@�p�@�r@�o�@�n@�m]@�m�@�j�@�qL@�p�@�p;@�o~@�o*@�n@�o�@�p�@�p;@�p�@�q�@�s@�r�@�r\@�qL@�q�@�qL@�r@�q�@�p�@�t�@�t�@�uy@�u@�u@�ti@�t�@�v!@�ti@�v�@�r�@�t�@�t�@�t�@�t*@�t*@�t�@�uO@�x�@�x�@�wp@�w@�w@�u�@�u�@�w�@�w�@�zN@�y>@�y�@�y�@�x�@�v`@�t�@�w�@�wp@�w�@�w�@�|1@�}�@�}�@�}�@�}A@�}A@�}A@�}�@�~R@�}�@�~R@�@�@�@�@�@�c@�@�~�@�~�@�~R@�c@�c@���@��0@���@���@���@��E@��@��@�@�~�@��0@���@�~�@�c@�|1@�{�@�{t@�}A@�~R@��E@���@��V@��E@���@���@���@��E@���@���@���@���@���@��@���@��@��4@�x@�~g@�x@�}�@�|1@�|1@�|F@�z�@�z�@�{5@�|�@�~(@��@��@��@�x@��@��4@�~�@�|�@�|�@�~@��4@��4@�}�@�|�@�y�@�y�@�y�@�{�@�z�@�z�@�z:@�y}@�z�@�zx@�|�@�|F@�|�@��@���@���@��'@���@���@��@���@���@��Y@���@���@���@��8@���@��8@��8@���@��'@���@�~�@��Z@��@��@��I@��I@�yh@�_@�_@�`B@�_�@�`B@�`B@�Y�@�\�@�]d@�]�@�]y@�[�@�]@�Y�@�VC@�Wi@�U�@�U�@�V@�U�@�V�@�V�@�Wi@�V�@�VX@�Wi@�W�@�W�@�X%@�Xy@�Xy@�X�@�Y6@�X�@�Yu@�Y6@�Y!@�X�@�Xy@�Xd@�Xy@�Xd@�X�@�Xd@�Xd@�Xd@�X@�W�@�W�@�V�@�VC@�Tv@�Tv@�R�@�O@�N�@�RT@�VC@�W @�VC@�V�@�V�@�V�@�V�@�VC@�Tv@�T"@�R�@�Se@�P�@�M�@�MU@�L�@�H�@�Ft@�=�@�+,@�#�@�!�@�")@�!�@�!�@�#�@�%@�%@�%@�%�@�%�@�&�@�&�@�&-@�%�@�&-@�&�@�'�@�'=@�'�@�'�@�'�@�'=@�'�@Q�@Qb@Q�@Qf@Q�@Q@Q�@Q�@QI@Q@Q@Q�@Q@Q@Q�@Q#@Q�@Q�@Q|@Q|@Q|@Q�@Q�@Q�@Q�@Q(@Q�@Q�@Q�@Q�@Q�@Q�@Q�@Q�@Q�@Q�@Q�@Q�@Q�@Q�@Q�@Q�@Q�@Q�@Q0@Q�@Q5@Q5@Q�@Q�@Q�@Q5@Q�@Q5@Q�@Q�@Qc@Q9@Q
�@Q
=@Q	�@Q�@Q@Q�@Q�@Q!@Q!@Q�@Q�@Q%@Q%@Q�@Q�@Q�@Q�@Q�@Q�@Q�@Q;@Q�@Q;@Q;@Q;@Q �@Q;@Q �@Q �@Q i@Q ?@Q i@P��@P�m@P�C@P�H@P��@P��@P��@P�o@P�@P��@P�@P��@P�Z@P��@P�5@P�@P�@P�9@P�>@P�B@P�@P��@P�F@P��@P�O@P��@P�`@P��@P�@P�?@���@���@��D@���@��4@��H@��]@���@��w@��@��@���@��,@��@��s@���@���@��{@��U@��@@���@��@���@���@���@��r@��H@��@��H@��H@��r@���@���@��]@��I@��#@��'@��@���@���@���@��(@��[@���@��@��9@��4@��'@���@���@���@���@���@��o@���@��V@��o@��@���@���@��|@��g@��k@��@��1@��1@��$@��9@��@���@��$@���@���@��$@���@���@���@���@�ł@�Ɠ@��;@���@��m@��C@���@�ı@��.@�ć@��H@��C@���@�õ@�õ@��;@�¤@��@�õ@���@��v@��+@��@���@��0@���@���@��4@��5@���@���@��N@��9@��@���@��[@��W@��@��@��u@��@��K@���@���@��F@��@���@���@��>@��p@���@���@��}@��B@���@���@���@��x@��@��>@��}@���@��x@���@��9@��x@��@��)@��}@��l@��)@��>@��}@���@��-@���@���@��@�� @��K@���@���@��>@��@���@���@���@���@��)@���@���@��m@��G@���@��u@��i@��;@�~�@�iY@�a�@�c�@�`�@�\}@�[�@�`@�a�@�a�@�a(@�`@�bx@�a�@�a�@�a�@�b9@�b�@�c�@�c�@�c�@�c�@�d0@�cs@�c�@QP3@QP	@QO�@QO@QN<@QN<@QN<@QLn@QK�@QK@QJ�@QJw@QI�@QJ�@QJ�@QIR@QI�@QI�@QI{@QI{@QH�@QH�@QHV@QH@QH,@QH,@QH,@QHV@QG�@QG�@QH@QH@QG�@QH@QH@QG�@QG�@QH@QGZ@QG@QGZ@QGZ@QG0@QG0@QG0@QGZ@QF�@QF@QE�@QE�@QEc@QEc@QE�@QE�@QE�@QE�@QE�@QE9@QE9@QD�@QD=@QC�@QB�@QA�@QA�@QA @QA @Q@�@Q@�@Q@y@Q@%@Q?�@Q?�@Q?S@Q>�@Q>-@Q<@Q;:@Q:�@Q:�@Q;@Q:�@Q:�@Q:�@Q:�@Q:�@Q:�@Q;@Q;@Q;:@Q;@Q;@Q:�@Q:�@Q:�@Q9�@Q6�@Q2�@Q0�@Q.@Q)�@Q,(@Q+@Q*0@Q)�@Q(�@Q)5@Q(�@Q'�@Q'@Q&�@Q#�@Q&l@Q#�@Q%�@Q#�@Q`@Q 2@Q!-@Q �G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             3433444344344444443443444444434443344444444434444444444444443443344444344443343334344443444443443444433444444444444433433443344334434444344444434344434444434444444334444444433434444444434433443443434343334443443334343344333344343334333334433333434333433333334333333333333333333333333333333333343333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333343333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�cG�O�@�
�@�d�G�O�G�O�G�O�@�;�G�O�G�O�@�\iG�O�G�O�G�O�G�O�G�O�G�O�G�O�@�d[G�O�G�O�@�]�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�JuG�O�G�O�G�O�@�]&@�]&G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�`G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��G�O�G�O�@�Y�@�`jG�O�G�O�G�O�G�O�G�O�@f��G�O�G�O�G�O�G�O�@�^H@QMG�O�@�cJ@�^�@�a�G�O�@dL`G�O�G�O�G�O�G�O�@�]�G�O�G�O�G�O�G�O�G�O�@�g&G�O�G�O�@���G�O�G�O�G�O�G�O�@�Jv@�MUG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�ek@�h�G�O�@�D�@�`G�O�G�O�@�h5@�^yG�O�G�O�@�g�@��CG�O�G�O�@��>G�O�G�O�G�O�G�O�@�c^G�O�G�O�G�O�G�O�G�O�G�O�@�]�G�O�@\%�G�O�G�O�G�O�@�L�G�O�G�O�G�O�G�O�G�O�@�i�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@Z�e@�iG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�]�@�a�G�O�@�]�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��YG�O�G�O�@�i@�`G�O�G�O�@�k�G�O�G�O�@�])G�O�@R�.G�O�@S hG�O�@�[�@�Z�@�W�G�O�G�O�G�O�@�a(G�O�G�O�@�WU@�T#@��G�O�@Mv]G�O�@�_�@�2zG�O�G�O�@��T@�e�@�d
@�jG�O�G�O�@�d�G�O�@�f'@�f'@�eG�O�@�i@�M@�h�@�i@�d�G�O�G�O�@�h5@�iB@�b�@i�J@Q�G�O�@�fG�O�@�g�@�g&@�L�G�O�@�i@�j�@�j�@�i@�k�@�h�@�g�G�O�@�i_@�i@�g�@�g�@�X�@�h�@�iB@�iG@�i�@�j�@�k�@�hH@�i
@�j�@�g�@�g<@X�@�hJ@�hG@�hH@�hJ@�i@�iV@�i@�i@�i�@�i]@�i]@�h�@�hK@��/@�i@�h�@�jG�O�@�h�@�g�@�g�@�hJ@�i_@�i�@�i@�i@�iV@�h�@�i@�h�@�h�@�g�@�hH@�i@�h�@�h�@�h�@�h�@�h�@�g�@�h5@�h�@�h5@�g�@�g�@�h5@�h5@�iB@�iB@�i�@�j@�jW@�k@�kz@�j�@�k*@�k�@�l�@�k�@�k�@�l7@�k&@�j@�i]@�i@�j@�k'@�k�@�k�@�l�@�h�@�i�@�hI@�g4@�i�@�i�@�j@�k�@�k*@�j�@�k�@�l7@�k&@�mc@�o{@�k*@�no@�o�@�n�@�o*@�o{@�n@�n�@�m�@�m6@�m6@�n]@�n�@�m7@�m�@�m6@�o @�m�@�m/@�m/@�k�@�l�@�l7@�m�@�j�@�n�@�nn@�mZ@�l�@�l�@�l�@�k�@�j@�h�@�m`@�m\@�nn@�m[@�m�@�m^@�m[@�m`@�l�@�l�@�k�@�l�@�j�@�l7@�k�@�i�@�i�@�j�G�O�@�k&@�j~@�mZ@�l�@�m�@�n�@�o{@�o�@�n�@�p9@�nj@�l�@�j~@�g�@�l�@�o�@�m�@�p�@�j@�g�@�i]@�k'@�eW@�n�@�p~@�py@�p@�oU@�p�@�rE@�nZ@�m�@�o�@�o@�n�@�o~@�o~@�p�@�qM@�p<@�o~@�m[@�n@�n�@�p�@�o�@�qM@�p�@�o�@�p�@�r@�q�@�q�@�p�@�p�@l)�@�q�@�r�@�r�@�q�@�q$@�q$@�r@�q�@�r\@�r	@�p�@�p�@�p�@�r
@�o�@�n@�m^@�m�@�j�@�qL@�p�@�p<@�o~@�o,@�n@�o�@�p�@�p8@�p�@�q�@�s@�r�@�r^@�qI@�q�@�qJ@�r@�q�@�p�@�t�@�t�@�u}@�u@�u@�tk@�t�@�v@�ti@�v�@�r�@�t�@�t�@�t�@�t*@�t)@�t�@�uO@�x�@�x�@�wr@�w@�w@�u�@�u�@�w�@�w�@�zQ@�y=@�y�@�y�@�x�@�va@�t�@�w�@�wt@�w�@�w�@�|/@�~@�}�@�}�@�}B@�}B@�}B@�}�@�~R@�}�@�~S@�@�@�@�@�@�d@�@�~�@�~�@�~T@�b@�b@���@��1@���@���@���@��H@���@��2@��L@��_@���@��x@��@��@���@��*@��@��u@���@���@��|@��V@��@@���@��@���@���@���@��s@��N@�� @��J@��L@��y@���@���@��_@��J@��&@��*@��@���@���@���@��)@��[@���@��@��;@��4@��$@���@���@���@���@���@��q@���@��W@��n@��@���@���@��~@��i@��l@��@��2@��0@��$@��9@��@���@��%@���@���@��$@���@���@���@���@�ł@�Ɩ@��:@���@��n@��H@���@�ı@��0@�ĉ@��J@��H@���@�ö@�ö@��=@�¤@��@�ö@���@��u@��/@��@���@��2@���@���@��1@��9@���@���@��N@��8@��
@���@��[@��Z@��@��@��v@��@��N@���@���@��D@��	@���@���@��@@��u@��@���@��|@��B@���@���@���@��z@��@��?@��|@���@��z@���@��7@��z@��@��)@��w@��n@��)@��@@��y@���@��.@���@���@��@��@��J@���@���@��@@��@���@���@���@���@��)@���@���@��o@��G@���@��x@��l@��>@�~�@�iZ@�a�@�c�@�a@�\�@�\@�`@�a�@�a�@�a*@�`@�bz@�a�@�a�@�a�@�b9@�b�@�c�@�c�@�c�@�c�@�d2@�cv@�c�@QP3@QP@QO�@QO@QN:@QN=@QN>@QLk@QK�@QK @QJ�@QJz@QI�@QJ�@QJ�@QIM@QI�@QI�@QI{@QIz@QH�@QH�@QHZ@QH@QH0@QH.@QH-@QHS@QG�@QG�@QH @QH@QG�@QH@QH@QG�@QG�@QG�@QGX@QG
@QG[@QG[@QG5@QG5@QG3@QGX@QF�@QF@QE�@QE�@QE`@QEc@QE�@QE�@QE�@QE�@QE�@QE:@QE8@QD�@QD=@QC�@QB�@QA�@QA�@QA @QA @Q@�@Q@�@Q@z@Q@%@Q?�@Q?�@Q?V@Q>�@Q>0@Q<@Q;=@Q:�@Q:�@Q;@Q:�@Q:�@Q:�@Q:�@Q:�@Q:�@Q;@Q;@Q;:@Q;@Q;@Q:�@Q:�@Q:�@Q9�@Q6�@Q2�@Q0�@Q."@Q)�@Q,*@Q+@Q*-@Q)�@Q(�@Q):@Q(�@Q'�@Q'@Q&�@Q#�@Q&p@Q#�@Q%�@Q#�@Qb@Q 2@Q!.@Q �@���@���@��H@���@��2@��L@��_@���@��x@��@��@���@��*@��@��u@���@���@��|@��V@��@@���@��@���@���@���@��s@��N@�� @��J@��L@��y@���@���@��_@��J@��&@��*@��@���@���@���@��)@��[@���@��@��;@��4@��$@���@���@���@���@���@��q@���@��W@��n@��@���@���@��~@��i@��l@��@��2@��0@��$@��9@��@���@��%@���@���@��$@���@���@���@���@�ł@�Ɩ@��:@���@��n@��H@���@�ı@��0@�ĉ@��J@��H@���@�ö@�ö@��=@�¤@��@�ö@���@��u@��/@��@���@��2@���@���@��1@��9@���@���@��N@��8@��
@���@��[@��Z@��@��@��v@��@��N@���@���@��D@��	@���@���@��@@��u@��@���@��|@��B@���@���@���@��z@��@��?@��|@���@��z@���@��7@��z@��@��)@��w@��n@��)@��@@��y@���@��.@���@���@��@��@��J@���@���@��@@��@���@���@���@���@��)@���@���@��o@��G@���@��x@��l@��>@�~�@�iZ@�a�@�c�@�a@�\�@�\@�`@�a�@�a�@�a*@�`@�bz@�a�@�a�@�a�@�b9@�b�@�c�@�c�@�c�@�c�@�d2@�cv@�c�@QP3@QP@QO�@QO@QN:@QN=@QN>@QLk@QK�@QK @QJ�@QJz@QI�@QJ�@QJ�@QIM@QI�@QI�@QI{@QIz@QH�@QH�@QHZ@QH@QH0@QH.@QH-@QHS@QG�@QG�@QH @QH@QG�@QH@QH@QG�@QG�@QG�@QGX@QG
@QG[@QG[@QG5@QG5@QG3@QGX@QF�@QF@QE�@QE�@QE`@QEc@QE�@QE�@QE�@QE�@QE�@QE:@QE8@QD�@QD=@QC�@QB�@QA�@QA�@QA @QA @Q@�@Q@�@Q@z@Q@%@Q?�@Q?�@Q?V@Q>�@Q>0@Q<@Q;=@Q:�@Q:�@Q;@Q:�@Q:�@Q:�@Q:�@Q:�@Q:�@Q;@Q;@Q;:@Q;@Q;@Q:�@Q:�@Q:�@Q9�@Q6�@Q2�@Q0�@Q."@Q)�@Q,*@Q+@Q*-@Q)�@Q(�@Q):@Q(�@Q'�@Q'@Q&�@Q#�@Q&p@Q#�@Q%�@Q#�@Qb@Q 2@Q!.@Q �G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             3433444344344444443443444444434443344444444434444444444444443443344444344443343334344443444443443444433444444444444433433443344334434444344444434344434444434444444334444444433434444444434433443443434343334443443334343344333344343334333334433333434333433333334333333333333333333333333333333333343333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333343333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�9�Z�9�]�9�\F9�[�9�[29�[L9�[_9�Y�9�Zz9�\9�\9�Z�9�X19�W9�V9�V�9�T�9�Y�9�]R9�]<9�\�9�Z9�Z�9�[�9�[�9�[s9�[N9�[!9�[J9�[L9�[y9�Z�9�Z�9�[_9�VU9�Z)9�Y/9�W"9�Y9�V�9�T�9�T89�Rn9�R�9�V)9�UH9�[49�Y)9�Z�9�X�9�Z�9�Z�9�[�9�Wy9�U�9�X]9�Wv9�Z9�V�9�V9�T�9�Tw9�S}9�S(9�RE9�RC9�U19�UF9�R29�U�9�U29�T�9�S�9�U19�Y�9�[�9�\�9�]�9�au9�b�9�c*9�b�9�aa9�a<9�a�9�`�9�a$9�`~9�`@9�a<9�`�9�_�9�_�9�^79�^�9�_9�_�9�_�9�_m9�@g9�8V9�769�:w9�;�9�:99�9x9�4�9�769�699�8�9�8�9�4\9�8�9�0�9�1�9�/i9�/i9�/�9�0`9�/�9�0�9�0P9�0�9�0c9�/�9�2/9�2�9�0�9�2Y9�3%9�2�9�1�9�3"9�2�9�3%9�3�9�3d9�2�9�2�9�3M9�3�9�4�9�3�9�3�9�2l9�2~9�2�9�1�9�2~9�2�9�2�9�1�9�1�9�19�1�9�*t9�-c9�/�9�1F9�29�2�9�3c9�2S9�3S9�39�29�2~9�.�9�0N9�,�9�+�9�-M9�*�9�(�9�)�9�k9�9��Q9� ]9���9��M9���9���9���9���9���9���9��;9���9���9���9���9���9� �9� �9� �9� �9� �9� 59� q9O�U9O�.9O��9O�59O�`9O�c9O�d9O��9O��9O�M9O��9O��9O�*9O��9O��9O�}9O��9O��9O��9O��9O�/9O��9O��9O�69O�c9O�a9O�`9O��9O�9O��9O�39O�59O�9O�;9O�69O�
9O��9O�19O��9O�?9O��9O��9O�j9O�j9O�h9O��9O��9O�D9O��9O��9O��9O��9O��9O��9O��9O��9O��9O�s9O�q9O� 9O�x9O��9O��9O�39O��9O�a9O�a9O�9O�9O��9O�h9O��9O��9O��9O��9O�x9O�W9O��9O�69O�99O�`9O�9O�49O�69O�39O�49O�49O�\9O�`9O��9O�c9O�Y9O�69O�9O��9O�9O�)9O�E9O 9O|�9Ow�9Oz�9Oyq9Ox�9OxO9Ov�9Ow�9Ow,9Ov\9Ou�9Oub9Ort9Ot�9OrI9Ot:9Or$9Om�9On�9Oo�9OoG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B��B��B��B��B��B��B�jB�dB�dB�dB�XB�LB�FB�FB�LB�LB�LB�LB�LB�RB�RB�RB�XB�XB�^B�qB��B�B�ZB�;B��B�NB��B]/Bn�B��B��B�B�#B�BB�HB�B�B��B�B�B�B\)Bl�Bu�Bw�Bs�Bs�B{�Bz�By�B|�B� B� B|�B� B�+B�B�B}�Bq�Bo�Be`B]/BVBK�BF�B7LB.B'�B%�B �B�BhBDBB��B�B�
B��B�wB��B�JBy�BaHBM�BL�B@�B,BPBĜB��B��B�Br�B]/B5?B�BB
�B
�TB
�#B
��B
ŢB
��B
�VB
q�B
ffB
I�B
0!B
�B
PB
B	�B	��B	�dB	��B	��B	�VB	�B	s�B	ffB	ZB	@�B	&�B	�B	oB	B��B�B�;B�
B��BB��B�wB�jB�XB�FB�-B�B�B��B��B��B�B�!B�!B�B�B�B�B�B��B��B��B��B��B��B��B��B��B�oB�bB�VB�JB�=B�1B�%B�Bz�Bw�Bu�Br�Bp�Bn�Bm�Bk�BiyBhsBffBe`BcTBaHB_;B]/BYBW
BVBR�BN�BJ�BH�BD�BA�B>wB>wB>wB=qB=qB:^B8RB8RB6FB5?B6FB5?B2-B1'B6FB49B49B49B5?B49B33B2-B0!B1'B1'B2-B2-B1'B0!B/B/B/B.B-B,B+B+B+B,B,B,B-B-B,B,B.B/B/B/B0!B/B/B.B/B0!B0!B1'B1'B1'B0!B0!B0!B/B1'B2-B33B49B49B5?B6FB8RB8RB9XB:^B=qBD�BP�BO�BI�BG�BF�BF�BF�BH�BM�BR�BT�BYBYBZBZBW
BVBVBXB\)BaHBffBl�Bn�Bo�Bq�Br�Bt�Bu�B|�B�B�+B�1B�=B�\B�\B��B�uB�{B��B��B�-B�FB�^BBǮB��B��B��B�
B�B�BB�yB�B�B��B��B��B��B��B��B	  B	hB	�B	�B	�B	�B	 �B	"�B	#�B	&�B	'�B	(�B	)�B	+B	.B	/B	0!B	2-B	49B	5?B	8RB	:^B	?}B	A�B	B�B	D�B	I�B	K�B	Q�B	VB	YB	ZB	ZB	\)B	^5B	aHB	bNB	bNB	cTB	e`B	e`B	e`B	ffB	gmB	gmB	hsB	iyB	jB	jB	k�B	q�B	u�B	z�B	}�B	� B	�B	�B	�B	�B	�B	�+B	�7B	�=B	�=B	�=B	�JB	�VB	�oB	�bB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�3B	�?B	�LB	�XB	�RB	�XB	�dB	�jB	�qB	�}B	��B	��B	��B	��B	B	ÖB	ŢB	ƨB	ǮB	ǮB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�
B	�
B	�B	�B	�B	�B	�#B	�)B	�/B	�5B	�5B	�5B	�;B	�HB	�HB	�NB	�NB	�TB	�ZB	�`B	�fB	�fB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B
 B
�B
SB
 �B
)B
1�B
:B
@�B
F�B
K�B
Q�B
W�B
\CB
`�B
e`B
h�B
l"B
p!B
s�B
x�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�	?:��B��B��?��f?�|�A���B^K@D�f?�By>���?&�OA��L@>Κ>��?+ �@�|9B�I?!��@_B�A�I?s~?WG�?�x6@��(?!�@T>6Bk�>�%�?\�?x�Bv�B�b?T3@���@C��>���>�b�>�.U>�5"?7j�A��Bz�@�m>���>�p?č�?"'�@vQ"A�O�?v?�m�>ͪ?`>ނ�?��@��@(GkA�/@>�6i?:�BuGB��>���>�^�?C�}@l��?G;�A�K? ��AK.?*��A)�BxpA��v?���By)Bv�B�?��A���@��>���?"K�A_�B�8>�UL>�aF?s�?�As�vB��@h�@�A�?	xp?S<>A��@��7Bd�B�f>��>|i�>�-�>��g>���>�Y>�%�?2z�A�@팰>�%J?�`?�� B}nB�{?D؈A��oB�Q?�g@�M�B��B�>�2�?S��B��BƑ?#X:@}�}B5�>���>��? "�?�mPB��?{��@���?�A ��@�D�@-��Bz&?Hh�A���?���?
??I�dB�?\"�@���@�.�?ʟ?k4aB��?)I�@Gp�>�=?��A��?6U�?/��A��B�D@�r�A�@�[�?��5A[�@�?o��AhGB{�B��?K�B��?7v?=LfA�Z�?�m?m�Ag&�?U2?;�yA���?7@�UB��B~�?(�=@בB�@?�@@7�B�W?J��A��?ER	A�Ԑ?g\KB{�BTB|�>�|�?'�W@��B� ?L#@SrGByBv�A�^�?LoA�`�?���B~sB6�@��W?���A��B��B��Bs?j�?�Z�B��A<��B�B�B�n?� B�MB=�B��B�MB�>�Fg@;��B��B�~B~=A���A���@�ŗB��AV��B��B�$BF@4!qB�B�"B�+B�B�SB��B�^@s*B��B�DB�zB��Bu�B�\B�GB��B�)B��B�>B�)B�tB�IB��B��A���B��B��B�)B�XB�MB�ZB�B�0B��B��B��B��B��B�VB�MB��B�>A�2�B��B��B�B��B��B��B�B�DB�ZB��B�B��B� B�B�)B�B��B��B��B��B��B�B�B��B�MB��B� B�B�9B��B��B�+B�VB��B��B��B�ZB��B�SB��B�B�B��B�(B�B��B�DB��B�8B�B�B��B��B�)B��B�gB��B�)B�>B��B�0B��B�B��B��B�vB�B�0B��B�B�#B��B�B�>B�GB�B��B�GB�B�GB��B��B��B��B�3B��B��B��B��B��B�1B�+B�ZB��B��B�gB�MB��B��B�uB�"B��B��B��B��B�)B�B��B��B��B�MB��B�MB�+B��B�>B~>B��B��@4�jB��B�B��B��B��B�#B�B�6B�ZB��B�B��B�B��B� B��B��B��B��B�KB�+B�8B��B�rB�XB��B�'B��B�rB��B��B��B�pB��B��B��B��B�:B��B��B��B�$B��B��B�B�FB��B��B�\B�B��B�EB�MB�vB��A�tPB�B�OB�OB�kB��B��B�cB�B��B�ZB�iB�2B��B��B�TB�GB�GB�\B�B�~B��B��B�iB�TB�GB��B��B�|B�aB�xB��B�~B�B��B��B�GB��B��B�eB��B�rB��B��B�>B�5B��B�/B��B�B��B��B�:B��B��B�VB��B��B��B��B��B�B��B��B�B��B��B��B��B��B��B��B�>B��B�_B�B��B��B�\B�B�hB�)B�gB�/B�AB�wB��B��B��B�~B��B��B�B��B��B��B�$B�B��B��B��B��B�}B�LB�B�B��B��B��B�PB��B��B�B��B�lB��B��B��B�jB�TB�mB��B�,B��B��B��B�FB�SB��B��B�}B�uB��B�4B�bB��B��B�2B��B�sB�B��B�B�B�)B��B�]B�@B�oB��B�XB��B�NB��B��B�'B�NB�hB�XB��B��B��B�QB�rB�B��B��B�	B�B�B�&B��B��B�2B�B�!B��B��B�HB��B�B��B�mB�B��B�qB�B��B�B�%B��B��B�|B��B�:B��B��B��B��B��B��B��B|B�B��B��B�B�B��B~9B��B��B�B��B�9B��B�B��B��B�~B�?B��B�.B�B�!B��B�fB��B��B�,B�7B��B��B��B�,B��B�&B��B�vB��B�HB��B��B��B�tB��B�dB�%B�TB��B�_B�OB��B��B�B�,B��B~�B�~B��B�VB�#B�7B�|B��B�	B��B�B�B�B~�B��B�B~�B{�B{3Bz�BydBv.BkEBtB�9B��B��B��B�JB�B��B�B�rB�B~�B}6B<B�pB�B~�B�B�B�B~�B�B�BcB	��B	�>B	��B	�~B	��B	�&B	��B	�pB	�B	��B	��B	ÇB	��B	��B	��B	��B	½B	£B	�:B	�-B	�B	�]B	�PB	�qB	�fB	B	�-B	�B	��B	��B	�B	��B	��B	§B	B	ÐB	�sB	½B	��B	µB	��B	�{B	B	�TB	�	B	��B	�:B	�-B	��B	�`B	��B	��B	°B	��B	B	�>B	�B	��B	B	�B	°B	��B	�rB	�B	�B	B	B	�0B	�3B	«B	áB	�7B	¯B	�uB	��B	��B	�VB	�B	��B	��B	ÜB	ÏB	ÂB	�7B	�ZB	�B	��B	B	�`B	�qB	�B	��B	�vB	��B	�RB	�<B	�FB	�fB	�NB	�/B	�QB	�B	ĮB	�UB	��B	��B	�YB	��B	�B	�gB	ţB	�$B	ƫB	�1B	�B	��B	�gB	�~B	��B	��B��B�xB��B��B�TB�WB�*B��B�?B�%B��B��B� B�B�=B�fB�ZB��B��B��B��B�B��B��B��B�`B�*B��B��B�	B��B�lB�=B�|B��B�DB��B��B��B��B��B�B��B�7B�!B�>B��B��B¢B�>B��B�B�eB�)B�uB�B�?B��B�}B� B��B�cB�uB��B��B��B��B��B��B��B��B�CB�WB��B�9B��B��B�B��BB��B��B�)B��B��B�cB��B��B��B��B�tB��B�B�tB��B�#B��B��B�+B�B�jB�B�!B�<B��B�}B��B��B�B�AB�&B�mB�#B�B�-B��B��B�B��B��B��B�'B��B�)B��B�&B�~B��B�+B�B��B�^B�0B��B��B��B�
B�B�sB�B�xB�jB�.B�)B��B��B��B��B��B��B� B��B�OB��B�_B�\B�B�,B��B��B�-B�B��B�0B�xB�RB��B��B�SB�5B��B��B��B��B�:B��B��B��B�pB�$B��B�5B�B�B��B�B��B��B��B��B��B�B��B��B��B��B��B�B�MB�wB	�_B	�3B	��B	�dB	��B	��B	�B	�gB	��B	�gB	��B	��B	�lB	��B	�B	��B	��B	��B	�B	�B	�3B	��B	�B	�cB	�hB	�[B	�MB	�_B	��B	��B	��B	��B	�B	��B	�B	�B	�VB	�B	�B	�B	��B	��B	�B	�B	�B	�B	��B	�B	�@B	�%B	��B	��B	��B	��B	��B	�B	��B	�WB	�IB	�B	�zB	�!B	�<B	�B	�}B	�B	�B	�B	�B	�gB	�B	�B	�B	�_B	��B	�pB	��B	�@B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�zB	�B	�_B	�EB	�B	��B	�B	�B	�B	�MB	��B	��B	��B	�B	�B	�B	�B	��B	�)B	��B	�-B	�B	�jB	�GB	��B	��B	�DB	��B	��B	�iB	��B	�_G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111199999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993433444344344444443443444444434443344444444434444444444444443443344444344443343334344443444443443444433444444444444433433443344334434444344444434344434444434444444334444444433434444444434433443443434343334443443334343344333344343334333334433333434333433333334333333333333333333333333333333333343333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333343333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 B�~B��B��B�~B�xB�|B�~B�cB�ZB�ZB�]B�OB�CB�=B�@B�EB�FB�CB�CB�FB�IB�JB�GB�PB�NB�WB�eB�~B�B�RB�1B˿B�EB��B]$Bn�B��B��B�B�B�7B�@B�B��B��B�B�B�B\"Bl�Bu�Bw�Bs�Bs�B{�Bz�By�B|�B�B�B|�B�B�!B��B�B}�Bq�Bo�BeXB]&BU�BK�BF�B7DB.B'�B%�B �B�B[B<BB��B�{B��B��B�mB��B�?By�Ba=BM�BL�B@{B+�BFBĐB��B�}B�Br�B]#B52B�BB
�B
�HB
�B
��B
řB
��B
�HB
q�B
f]B
I�B
0B
�B
BB
B	�B	��B	�XB	��B	��B	�MB	�B	s�B	f[B	ZB	@wB	&�B	�B	eB	B��B�B�-B��BʶBB�wB�iB�^B�LB�:B�#B�B��B��B��B��B��B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B�yB�dB�UB�JB�>B�/B�$B�B��Bz�Bw�Bu�Br�Bp�Bn�Bm�BkxBilBhfBfZBeUBcJBa8B_-B]#BY	BV�BU�BR�BN�BJ�BH�BD�BA}B>jB>iB>jB=aB=bB:PB8FB8EB69B51B68B52B2B1B69B4*B4,B4*B52B4-B3%B2 B0B1B1B2B2 B1B0B/B/B/B.	B- B+�B*�B*�B*�B+�B+�B+�B- B-B+�B+�B.B/B/B/B0B/B/B.B/B0B0B1B1B1B0B0B0B/B1B2B3'B4)B4+B52B69B8BB8CB9HB:RB=cBD�BP�BO�BI�BG�BF�BF�BF�BH�BM�BR�BT�BYBY	BZBZBV�BU�BU�BXB\Ba7BfVBl{Bn�Bo�Bq�Br�Bt�Bu�B|�B�B�B�#B�-B�MB�MB�qB�gB�lB��B��B�B�9B�QB�BǡBʴB��B��B��B�B�4B�lB�~B�B��B��B��B��B��B��B��B	ZB	�B	�B	�B	�B	 �B	"�B	#�B	&�B	'�B	(�B	)�B	*�B	.B	/B	0B	2B	4+B	5/B	8CB	:OB	?nB	A{B	BB	D�B	I�B	K�B	Q�B	U�B	Y	B	ZB	ZB	\B	^&B	a<B	b@B	b@B	cDB	eSB	eRB	eRB	fYB	g_B	g^B	hcB	iiB	jrB	joB	kvB	q�B	u�B	z�B	}�B	�B	��B	�B	�B	�
B	�B	�B	�'B	�0B	�.B	�-B	�=B	�EB	�`B	�VB	�YB	�tB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�%B	�/B	�=B	�KB	�CB	�IB	�VB	�\B	�bB	�oB	�vB	�{B	�|B	�|B	B	ÆB	ŔB	ƚB	ǠB	ǢB	ȥB	ɬB	ɬB	˹B	˺B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	� B	�&B	�&B	�$B	�+B	�7B	�;B	�@B	�@B	�FB	�LB	�PB	�YB	�WB	�_B	�iB	�pB	�zB	�|B	�}B	�zB	�B	�B	�G�O�B	��B
 B
�B
FB
 �B
)B
1�B
: B
@�B
F�B
K�B
Q�B
W�B
\3B
`�B
eOB
h�B
lB
pB
s�B
x�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B� G�O�B��B��G�O�G�O�G�O�B^FG�O�G�O�ByG�O�G�O�G�O�G�O�G�O�G�O�G�O�B�?G�O�G�O�B�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�Bk�G�O�G�O�G�O�Bv�B�ZG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Bz�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�/1G�O�G�O�Bu@B��G�O�G�O�G�O�G�O�G�O�A�KG�O�G�O�G�O�G�O�BxeA��iG�O�ByBv�B�G�O�A���G�O�G�O�G�O�G�O�B�/G�O�G�O�G�O�G�O�G�O�B�zG�O�G�O�A�G�O�G�O�G�O�G�O�Bd�B�[G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B}gB�pG�O�A��]B�JG�O�G�O�B��B�G�O�G�O�B��BƈG�O�G�O�B5�G�O�G�O�G�O�G�O�B��G�O�G�O�G�O�G�O�G�O�G�O�BzG�O�A��G�O�G�O�G�O�B�G�O�G�O�G�O�G�O�G�O�B��G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�|B�=G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B{�B��G�O�B�xG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���G�O�G�O�B��B~�G�O�G�O�B�G�O�G�O�B�QG�O�A��G�O�A�ԃG�O�B{�BLB|�G�O�G�O�G�O�B�G�O�G�O�ByBv�A�^�G�O�A�`�G�O�B~lB6�G�O�G�O�A���B��B��BjG�O�G�O�B��G�O�B�B�B�cG�O�B�EB=�B��B�EB��G�O�G�O�B��B�rB~6A��zA���G�O�B��G�O�B��B�B<G�O�B�
B�B� B�
B�HB��B�WG�O�B��B�=B�qB��Bu�B�UB�:B�B� B��B�4B�B�mB�>B��B��A���B��B��B�B�OB�EB�PB�B�$B��B��B��B��B��B�RB�EB��B�2G�O�B��B��B�	B��B��B��B�B�=B�PB��B�
B��B��B�	B�B�
B��B��B��B��B��B�B�B��B�CB��B��B�B�0B��B��B�B�OB�yB��B�{B�OB��B�HB��B��B�B��B� B� B��B�=B��B�-B�B�B��B��B� B��B�\B��B� B�2B��B�)B��B��B��B��B�pB��B�)B��B�B�B��B��B�5B�=B�B��B�>B��B�=B��B��B��B��B�)B��B��B��B��B��B�)B� B�SB��B��B�_B�DB��B��B�kB�B��B��B��B��B� B�
B��B��B�wB�DB��B�DB�#B��B�4B~4B��B��G�O�B��B�B��B��B��B�B��B�-B�SB��B�	B��B�B�zB��B��B��B��B��B�@B�$B�-B��B�iB�OB��B�B��B�iB��B��B��B�gB��B��B��B��B�3B��B��B��B�B��B��B�B�=B��B��B�PB�B��B�9B�DB�nB��A�tBB�B�FB�FB�aB��B��B�ZB�B��B�PB�aB�%B��B��B�LB�;B�=B�TB�B�tB��B��B�`B�MB�;B��B�B�sB�XB�oB��B�wB�B��B��B�<B��B��B�YB��B�gB��B��B�6B�-B��B�#B��B�B��B��B�0B��B��B�LB��B��B��B��B��B�B�yB��B�
B��B��B��B��B��B��B��B�6B��B�TB�	B��B��B�PB�B�aB�B�^B�'B�:B�pB��B��B��B�uB��B��B��B��B��B�{B�B�B��B�wB�wB��B�uB�DB��B�mB��B��B�JB�OB�!B��B�5B�B��B��B�B��B�2B�]B�PB�xB��B��B��B�	B��B��B��B�VB�"B��B��B�B��B�aB�2B�tB��B�=B��B��B��B��B��B��B��B�,B�B�5B��B��BB�5B��B��B�\B�B�kB�B�5B��B�sB�B�~B�ZB�lB��B��B��B��B��B��B�zB��B�:B�NB��B�/B��B��B�B��B�}B�zB��B� B��B��B�YB��B��B��B��B�lB��B�B�kB��B�B��B�{B� B� B�`B�B�B�2B��B�rB��B��B�B�8B�B�bB�B�B�%B��B��B�B��B��B��B� B��B� B��B� B�wB��B�#B�B��B�SB�%B��B��B��B�B�B�jB�
B�pB�bB�"B� B��B��B��B��B��B��B��B��B�EB��B�XB�SB� B�!B��B��B�%B�B��B�(B�nB�KB��B��B�KB�,B��B��B��B��B�3B��B��B��B�fB�B��B�,B�B�B��B�B��B��B��B�{B��B��B��B��B��B��B��B��B�EB�oB	�OB	�%B	�B	�WB	�B	�B	�B	�VB	�B	�ZB	��B	��B	�]B	��B	�B	�B	��B	��B	�B	�B	�%B	��B	�B	�UB	�[B	�NB	�=B	�NB	��B	�B	��B	��B	�B	�B	�B	�tB	�JB	�uB	��B	�B	��B	��B	�B	�B	�zB	�B	��B	�}B	�2B	�B	��B	��B	��B	��B	��B	�B	��B	�JB	�;B	�B	�jB	�B	�,B	�B	�oB	�B	��B	�B	�B	�YB	�B	�B	�B	�RB	��B	�bB	��B	�2B	��B	��B	��B	��B	��B	��B	�B	�B	�sB	�B	�kB	�}B	�SB	�4B	�
B	��B	�B	�B	�B	�AB	��B	��B	�B	�sB	�B	��B	�B	�B	�B	�B	�B	�wB	�\B	�:B	��B	��B	�2B	�B	��B	�ZB	��B	�QB��B�mB��B��B�JB�OB�!B��B�5B�B��B��B�B��B�2B�]B�PB�xB��B��B��B�	B��B��B��B�VB�"B��B��B�B��B�aB�2B�tB��B�=B��B��B��B��B��B��B��B�,B�B�5B��B��BB�5B��B��B�\B�B�kB�B�5B��B�sB�B�~B�ZB�lB��B��B��B��B��B��B�zB��B�:B�NB��B�/B��B��B�B��B�}B�zB��B� B��B��B�YB��B��B��B��B�lB��B�B�kB��B�B��B�{B� B� B�`B�B�B�2B��B�rB��B��B�B�8B�B�bB�B�B�%B��B��B�B��B��B��B� B��B� B��B� B�wB��B�#B�B��B�SB�%B��B��B��B�B�B�jB�
B�pB�bB�"B� B��B��B��B��B��B��B��B��B�EB��B�XB�SB� B�!B��B��B�%B�B��B�(B�nB�KB��B��B�KB�,B��B��B��B��B�3B��B��B��B�fB�B��B�,B�B�B��B�B��B��B��B�{B��B��B��B��B��B��B��B��B�EB�oB	�OB	�%B	�B	�WB	�B	�B	�B	�VB	�B	�ZB	��B	��B	�]B	��B	�B	�B	��B	��B	�B	�B	�%B	��B	�B	�UB	�[B	�NB	�=B	�NB	��B	�B	��B	��B	�B	�B	�B	�tB	�JB	�uB	��B	�B	��B	��B	�B	�B	�zB	�B	��B	�}B	�2B	�B	��B	��B	��B	��B	��B	�B	��B	�JB	�;B	�B	�jB	�B	�,B	�B	�oB	�B	��B	�B	�B	�YB	�B	�B	�B	�RB	��B	�bB	��B	�2B	��B	��B	��B	��B	��B	��B	�B	�B	�sB	�B	�kB	�}B	�SB	�4B	�
B	��B	�B	�B	�B	�AB	��B	��B	�B	�sB	�B	��B	�B	�B	�B	�B	�B	�wB	�\B	�:B	��B	��B	�2B	�B	��B	�ZB	��B	�QG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111199999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993433444344344444443443444444434443344444444434444444444444443443344444344443343334344443444443443444433444444444444433433443344334434444344444434344434444434444444334444444433434444444434433443443434343334443443334343344333344343334333334433333434333433333334333333333333333333333333333333333343333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333343333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.11 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =-0.11 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =-0.11 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   none                                                                                                                                                                                                                                                            No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       202008311650292020083116502920200831165029202008311650292020083116502920200831165029202008311650292020083116502920200831165029202008311650292020083116502920200831165029AO  AO  AO  ARCAARCAARCAADJPADJPADJP                                                                                                                                                                                                            201902191817362019021918173620190219181736    IP  IP  IP                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201902191817362019021918173620190219181736  QCP$QCP$QCP$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�5F03E           103E            703E            AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201902191817362019021918173620190219181736  QCF$QCF$QCF$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�0               0               0               UW  UW  UW  ARSQARSQARSQUWQCUWQCUWQC            WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               202008311650292020083116502920200831165029  IP  IP  IP                                                  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                