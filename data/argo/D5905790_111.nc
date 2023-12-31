CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2021-04-26T19:34:21Z creation; 2022-09-06T18:14:19Z DMQC;      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.10   Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_on_dac_decoder_version        $Decoded by SIO: Argo SIO SOLOII V2.6   comment_dmqc_operator         bPRIMARY | https://orcid.org/0000-0003-0805-6570 | John Gilson, Scripps Institution of Oceanography        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    8   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  �  8   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  �  8�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  `  9   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        9x   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    9�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    9�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                  @  9�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    9�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    9�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                  @  9�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                  @  :   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                  @  :T   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    :�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   axis      T      
resolution        >�E�r�_K   
_FillValue        A.�~            :�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    :�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        ?F�k"kmj   
_FillValue        A.�~            :�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   	valid_min         �V�        	valid_max         @V�        axis      Y      
_FillValue        @�i�            :�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    	valid_min         �f�        	valid_max         @f�        axis      X      
_FillValue        @�i�            :�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    :�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    :�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    :�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    :�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    :�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    ;    CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        =    PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     �  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     �  cd   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     �  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �p   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �|   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �(   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 2�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � :4   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � X�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � `�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` @   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20210426193421  20220906181419  5905790 5905790 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               o   oAA  AOAO7824_008764_111                 7824_008764_111                 2C  2C  DD  SOLO_II                         SOLO_II                         8764                            8764                            V2.6; SBE602 06Mar19            V2.6; SBE602 06Mar19            853 853 @�o���	l@�o���	l11  @�o����@�o����@7���:э@7���:э�d�[l7a�d�[l7a11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�=q@�\@@  @}p�@��R@��R@�  A ��AG�A ��A,(�A@  A`  A�Q�A�Q�A��A�  A�  A�Q�A�Q�A�  B   B�
B  B  B   B(Q�B0(�B8  B@(�BH  BO�
BX  B`(�Bh  Bo�
Bw�
B�  B�(�B�(�B�{B�{B�{B�  B�{B�(�B�  B��B��B��B��B�  B�(�B�{B�{B�{B�{B�{B�(�B�  B��
B�  B��B��B�  B�  B�  B�{B�{C 
=C  C��C��C�C	��C  C
=C{C  C��C��C  C  C
=C
=C   C"  C$
=C%��C(  C*
=C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C?��CB  CD  CE��CG��CJ  CL  CN  CP  CR
=CT
=CV
=CX  CY��C\  C^  C`  Cb
=Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cq��Cs��Cv  Cx  Cy��C|  C~  C�C�  C�  C�C�C���C�  C�C�  C�  C���C�  C�  C�C�  C���C�  C���C���C���C�  C�  C�C�
=C�C�  C���C���C�  C�C�C�C�C�C�C�C���C���C�  C�  C�  C�  C���C���C�C�
=C�C���C�  C�  C���C���C���C�  C���C�  C�  C�  C�  C���C���C���C�  C�C�  C�  C�  C�  C�  C���C�  C�  C���C���C���C���C�  C�C�  C�  C�  C�  C�
=C�
=C�
=C�C�C�  C���C�C�C�  C�C�  C�  C�C�  C���C�C�  C���C�  C���C���C�C�  C���C�  C�C�C�  C���C���C���C�  C�  C�  C�
=C�C�C�  C���C���C�  C�C�C���C���C���D }qD �qD}qD��Dz�D  D��D�D� D  D� D  D��DD� D  D��D	D	��D
  D
}qD
�qD}qD�D��D  D}qD�qD� D�D}qD�qD� D  D}qD�D��D�D��D�D��D�D��DD��D�D� D  D� D  D� D  D� D�D��D  D� D  D� D�qD� D�D� D   D � D!  D!� D"�D"��D#�D#� D$  D$� D%  D%� D&  D&}qD&�qD'� D(�D(��D)  D)� D*�D*� D*�qD+� D,  D,� D-  D-}qD.�D.� D/  D/� D/�qD0� D1  D1��D1�qD2}qD3�D3��D4�D4��D5  D5� D6�D6� D6�qD7}qD8  D8}qD8��D9}qD9�qD:}qD;�D;��D<  D<� D=�D=��D>  D>� D?�D?� D?�qD@� DA  DA}qDB  DB� DB�qDC� DD�DD��DE�DE� DF  DF}qDG  DG��DH�DH� DI  DI}qDJ  DJ��DK  DK� DK�qDL}qDM  DM� DN  DN}qDN�qDOz�DP  DP��DQ  DQ}qDR  DR� DR��DS}qDT  DT� DU  DU}qDV  DV��DW�DW��DX  DX� DX�qDY}qDY��DZz�DZ��D[z�D[��D\z�D\�qD]� D^�D^� D^�qD_� D_�qD`}qDa  Da�Db�Db��Dc�Dc}qDc�qDd}qDd�qDe}qDf  Df��Dg  Dg� Dg�qDh� Di�Di��Dj  Dj� Dj�qDk� DlDl��Dm  Dm� Dm�qDn� Do  Do� Dp  Dp� Dq  Dq� Dq�qDr}qDr�qDs}qDs�qDt}qDu  Du� Dv  Dv� Dv��Dw}qDx  Dx��Dy  Dy� Dy�qDz}qDz�qD{� D|  D|��D}�D}� D}�qD~� D  D� D�  D�@ D�� D���D���D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�~�D�� D�HD�@ D�� D��HD�  D�@ D��HD�� D���D�>�D�~�D���D��qD�>�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�HD�AHD��HD��HD�  D�>�D�� D�� D�  D�AHD�� D�� D�HD�AHD�� D�� D�  D�>�D�~�D���D���D�@ D�� D��HD�  D�@ D��HD��HD�  D�>�D�~�D�� D���D�>�D�� D���D�  D�@ D�~�D�� D�  D�>�D�}qD���D�  D�@ D�� D���D���D�>�D�~�D���D���D�>�D�� D��HD��D�@ D�� D�� D�  D�@ D��HD��HD�  D�@ D�~�D���D���D�@ D��HD�� D���D�@ D�� D���D�  D�B�D��HD�� D�HD�@ D�~�D�� D�  D�>�D�� D�� D���D�AHD��HD�D�  D�>�D�~�D�� D�  D�@ D�� D�� D�HD�AHD���D��HD�  D�@ D�~�D�� D��D�AHD�~�D���D�  D�>�D�~�D�� D�  D�@ D�� D�� D�  D�>�D�~�D�� D���D�>�D��HD�� D�  D�>�D�~�D���D���D�>�D�� D��HD��D�@ D�~�D�� D�HD�>�D�}qD���D�  D�@ D�� D���D�  D�AHD�� D�� D�  D�>�D�~�D���D�  D�AHD��HD�D�HD�@ D�~�D���D�HD�AHD��HD�� D�HD�AHD�� D���D�  D�AHD�� D���D���D�>�D�� D��HD���D�@ D�� D���D�  D�AHD�� D��HD�HD�AHD��HD�� D�  D�@ D�� D�� D�HD�AHD�� D���D�  D�AHD��HD��HD�HD�@ D�~�D��qD�  D�@ D�~�D��HD�HD�AHD�� D�� D�HD�@ D�� D���D���D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ Dþ�D�  D�AHD�~�D�� D�HD�@ Dŀ D��HD�HD�AHDƁHD�� D�  D�AHDǀ D�� D�  D�@ DȁHD�� D�  D�AHDɀ D�� D��D�AHDʁHD�� D�  D�>�D�~�D�� D��D�AHD́HD�� D�  D�@ D̀ D�� D�  D�>�D�~�Dξ�D�  D�AHDπ D�� D���D�@ DЁHD�� D�  D�@ Dр D�� D�  D�AHDҀ D�� D�HD�@ D�~�D�� D�  D�>�D�~�D�� D���D�>�D�~�D�� D�  D�@ Dր D�� D���D�@ DׁHD��HD�  D�>�D�~�D��HD�  D�@ D�~�D�� D�HD�@ Dڀ Dھ�D���D�>�D�}qD۾�D�  D�AHD܁HD��HD�  D�>�D݀ D��HD�HD�AHDށHD�� D��qD�>�D߀ D�� D�  D�>�D�~�DྸD��qD�>�D�~�D�� D�  D�>�D�~�D�� D���D�@ D� D��HD�HD�AHD�~�D侸D�  D�>�D�~�D��HD�HD�AHD�HD�� D���D�>�D� D��HD�  D�@ D�~�D辸D�HD�AHD�HD�� D�  D�AHD� D�� D�HD�@ D�~�D�� D�HD�AHD� D쾸D���D�@ D� D��qD�  D�@ D� D�D�  D�=qD�~�D�� D�  D�AHD��HD��HD�HD�AHD� D�qD���D�@ D� D�D�HD�@ D�~�D�� D���?.{?aG�?�z�?�{?Ǯ?��@�@z�@&ff@8Q�@J=q@Y��@k�@}p�@�ff@���@�Q�@�  @���@���@��H@��
@���@�@޸R@���@��@��HAG�AA
=qA�RA33A�A(�A ��A%�A)��A.�RA2�\A7
=A;�A@��ADz�AHQ�AL��AQ�AUAY��A^{Ab�\AfffAj�HAn�RAs33Aw�A|(�A�Q�A��\A���A��RA�G�A�33A��A��A���A�(�A�ffA���A��HA��A�\)A�G�A��
A�A�Q�A��\A�z�A�
=A���A�33A�p�A�\)A��A�(�A��RA�Q�A��HA��A�\)A�G�A��
A�{A�Q�Aҏ\A���AָRA�G�AۅA�p�A߮A�=qA�(�A�RA���A��HA��A�\)A��A�(�A�ffA���A��HA�p�A��B ��B�B33B(�BG�BffB�B��B	B
�HB  BG�BffB\)Bz�B��B�RB  B��B{B
=B(�Bp�B�\B�B��BB�HB   B!�B"ffB#\)B$z�B%��B&�HB((�B)�B*ffB+\)B,��B-�B/
=B0  B1G�B2�\B3�B4��B5�B733B8Q�B9G�B:�\B;�
B=�B>=qB?\)B@z�BA��BB�HBC�
BE�BF=qBG�BH��BIBK
=BL(�BMp�BN�\BO�BQ�BR=qBS\)BT��BUBW
=BX(�BYG�BZ�RB[�
B]�B^ffB_�B`��Ba�Bc\)Bdz�Be��Bf�HBh  Bi�BjffBk�Bl��Bm�Bo\)Bp��BqBr�HBt(�Bup�Bv�RBw�
By�Bz=qB{\)B|��B~{B33B�(�B���B�\)B��B���B�33B��
B�z�B���B��B�Q�B��HB��B�{B��RB�\)B��B��\B�33B��
B�ffB�
=B���B�=qB��HB�p�B�{B��RB�G�B��B�z�B��B�B�ffB���B��B�(�B���B�\)B�  B��\B�G�B�B�z�B�
=B���B�=qB���B�p�B�  B���B�33B��
B�z�B���B��B�(�B��HB�\)B�  B���B�G�B�B�ffB���B��B�=qB��HB�p�B�(�B��RB�\)B��B��\B�33B�B�Q�B�
=B���B�=qB���B�p�B�  B���B�G�B��B��\B��B�B�ffB���B���B�(�B��RB�p�B��B���B�33B��
B�z�B��B��B�Q�B���B�p�B�{B��RB�\)B��B\B�33B��
B�ffB�
=BŮB�=qB��HBǅB�(�Bȣ�B�\)B��Bʣ�B��B�B�ffB�
=B͙�B�=qB��HBυB�{BиRB�\)B�  Bң�B�33B��
B�z�B�
=BծB�Q�B��HBׅB�{BظRB�\)B�  Bڣ�B�G�B��
B�z�B��B�B�z�B���B߮B�Q�B���B�B�=qB���B�B�(�B�RB�\)B�{B��B�\)B��B�\B�33B��
B�ffB��B뙚B�Q�B��HB홚B�(�B���B�\)B�{B��B�33B��
B�z�B��B�B�Q�B���B���B�=qB���B�\)B�{B���B�G�B��B��\B�33B�B�ffB�
=B��B�=qB��HB��C {C \)C ��C ��C=qC��C�C=qC�C�
C(�Cp�C��C{Cp�C�C
=CQ�C��C��C=qC��C�HC33C�CC
=CffC�C	
=C	\)C	��C	��C
Q�C
��C
�C=qC�\C�HC(�Cz�CC{C\)C��C��C33C�C�RC��C33Cp�C��C�
C  C(�C\)C�C�RC�C{C=qCffC��C��C��C(�CQ�Cz�C�C�
C
=C33C\)C�\C�RC�C{C=qCffC�\CC��C�CG�Cp�C��C�
C
=C33CffC�\C�RC��C�C\)C�C�C�HC{C=qCp�C��C�
C  C33CffC�\CC��C�CQ�Cz�C�RC�
C{C33CffC��CC��C�CQ�Cz�C��C�
C  C(�C\)Cz�C�C�HC  C=qC\)C�\CC�C{CG�CffC��C��C   C (�C \)C �C �RC �C!�C!Q�C!�C!�C!�HC"{C"=qC"p�C"��C"�
C#  C#33C#ffC#�\C#�RC#��C$�C$G�C$z�C$��C$�
C%
=C%33C%p�C%��C%��C%��C&(�C&Q�C&�C&C&�C'{C'G�C'p�C'��C'�
C(  C((�C(\)C(�C(�RC(�C){C)=qC)p�C)��C)��C*  C*(�C*\)C*�\C*�RC*�C+�C+G�C+z�C+��C+�
C,
=C,(�C,\)C,�\C,C,�C-�C-G�C-z�C-�C-�HC.
=C.=qC.p�C.��C.��C/  C/33C/\)C/�\C/�RC/�C0�C0G�C0�C0��C0�
C1  C133C1ffC1��C1C1��C2(�C2G�C2z�C2�C2�HC3{C3=qC3ffC3��C3C4  C433C4\)C4�\C4C4��C5(�C5Q�C5�\C5C5�C6(�C6\)C6��C6C7  C7(�C7ffC7��C7��C8
=C8=qC8p�C8��C8�
C9{C9G�C9z�C9�C9�HC:�C:Q�C:�C:C:��C;(�C;\)C;�\C;C<  C<=qC<p�C<��C<�HC={C=G�C=�C=�RC=�C>(�C>ffC>�\C>�
C?
=C?G�C?z�C?�C?�C@(�C@\)C@��C@��CA
=CA=qCAz�CA�RCA�CB�CBQ�CB�CBCC  CC(�CCffCC��CC�
CD
=CD=qCDz�CD�CD�CE�CEQ�CE�\CE�
CF
=CF=qCFz�CF�CF�CG(�CGffCG��CG�HCH{G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                            ?�=q@�\@@  @}p�@��R@��R@�  A ��AG�A ��A,(�A@  A`  A�Q�A�Q�A��A�  A�  A�Q�A�Q�A�  B   B�
B  B  B   B(Q�B0(�B8  B@(�BH  BO�
BX  B`(�Bh  Bo�
Bw�
B�  B�(�B�(�B�{B�{B�{B�  B�{B�(�B�  B��B��B��B��B�  B�(�B�{B�{B�{B�{B�{B�(�B�  B��
B�  B��B��B�  B�  B�  B�{B�{C 
=C  C��C��C�C	��C  C
=C{C  C��C��C  C  C
=C
=C   C"  C$
=C%��C(  C*
=C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C?��CB  CD  CE��CG��CJ  CL  CN  CP  CR
=CT
=CV
=CX  CY��C\  C^  C`  Cb
=Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cq��Cs��Cv  Cx  Cy��C|  C~  C�C�  C�  C�C�C���C�  C�C�  C�  C���C�  C�  C�C�  C���C�  C���C���C���C�  C�  C�C�
=C�C�  C���C���C�  C�C�C�C�C�C�C�C���C���C�  C�  C�  C�  C���C���C�C�
=C�C���C�  C�  C���C���C���C�  C���C�  C�  C�  C�  C���C���C���C�  C�C�  C�  C�  C�  C�  C���C�  C�  C���C���C���C���C�  C�C�  C�  C�  C�  C�
=C�
=C�
=C�C�C�  C���C�C�C�  C�C�  C�  C�C�  C���C�C�  C���C�  C���C���C�C�  C���C�  C�C�C�  C���C���C���C�  C�  C�  C�
=C�C�C�  C���C���C�  C�C�C���C���C���D }qD �qD}qD��Dz�D  D��D�D� D  D� D  D��DD� D  D��D	D	��D
  D
}qD
�qD}qD�D��D  D}qD�qD� D�D}qD�qD� D  D}qD�D��D�D��D�D��D�D��DD��D�D� D  D� D  D� D  D� D�D��D  D� D  D� D�qD� D�D� D   D � D!  D!� D"�D"��D#�D#� D$  D$� D%  D%� D&  D&}qD&�qD'� D(�D(��D)  D)� D*�D*� D*�qD+� D,  D,� D-  D-}qD.�D.� D/  D/� D/�qD0� D1  D1��D1�qD2}qD3�D3��D4�D4��D5  D5� D6�D6� D6�qD7}qD8  D8}qD8��D9}qD9�qD:}qD;�D;��D<  D<� D=�D=��D>  D>� D?�D?� D?�qD@� DA  DA}qDB  DB� DB�qDC� DD�DD��DE�DE� DF  DF}qDG  DG��DH�DH� DI  DI}qDJ  DJ��DK  DK� DK�qDL}qDM  DM� DN  DN}qDN�qDOz�DP  DP��DQ  DQ}qDR  DR� DR��DS}qDT  DT� DU  DU}qDV  DV��DW�DW��DX  DX� DX�qDY}qDY��DZz�DZ��D[z�D[��D\z�D\�qD]� D^�D^� D^�qD_� D_�qD`}qDa  Da�Db�Db��Dc�Dc}qDc�qDd}qDd�qDe}qDf  Df��Dg  Dg� Dg�qDh� Di�Di��Dj  Dj� Dj�qDk� DlDl��Dm  Dm� Dm�qDn� Do  Do� Dp  Dp� Dq  Dq� Dq�qDr}qDr�qDs}qDs�qDt}qDu  Du� Dv  Dv� Dv��Dw}qDx  Dx��Dy  Dy� Dy�qDz}qDz�qD{� D|  D|��D}�D}� D}�qD~� D  D� D�  D�@ D�� D���D���D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�~�D�� D�HD�@ D�� D��HD�  D�@ D��HD�� D���D�>�D�~�D���D��qD�>�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�HD�AHD��HD��HD�  D�>�D�� D�� D�  D�AHD�� D�� D�HD�AHD�� D�� D�  D�>�D�~�D���D���D�@ D�� D��HD�  D�@ D��HD��HD�  D�>�D�~�D�� D���D�>�D�� D���D�  D�@ D�~�D�� D�  D�>�D�}qD���D�  D�@ D�� D���D���D�>�D�~�D���D���D�>�D�� D��HD��D�@ D�� D�� D�  D�@ D��HD��HD�  D�@ D�~�D���D���D�@ D��HD�� D���D�@ D�� D���D�  D�B�D��HD�� D�HD�@ D�~�D�� D�  D�>�D�� D�� D���D�AHD��HD�D�  D�>�D�~�D�� D�  D�@ D�� D�� D�HD�AHD���D��HD�  D�@ D�~�D�� D��D�AHD�~�D���D�  D�>�D�~�D�� D�  D�@ D�� D�� D�  D�>�D�~�D�� D���D�>�D��HD�� D�  D�>�D�~�D���D���D�>�D�� D��HD��D�@ D�~�D�� D�HD�>�D�}qD���D�  D�@ D�� D���D�  D�AHD�� D�� D�  D�>�D�~�D���D�  D�AHD��HD�D�HD�@ D�~�D���D�HD�AHD��HD�� D�HD�AHD�� D���D�  D�AHD�� D���D���D�>�D�� D��HD���D�@ D�� D���D�  D�AHD�� D��HD�HD�AHD��HD�� D�  D�@ D�� D�� D�HD�AHD�� D���D�  D�AHD��HD��HD�HD�@ D�~�D��qD�  D�@ D�~�D��HD�HD�AHD�� D�� D�HD�@ D�� D���D���D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ Dþ�D�  D�AHD�~�D�� D�HD�@ Dŀ D��HD�HD�AHDƁHD�� D�  D�AHDǀ D�� D�  D�@ DȁHD�� D�  D�AHDɀ D�� D��D�AHDʁHD�� D�  D�>�D�~�D�� D��D�AHD́HD�� D�  D�@ D̀ D�� D�  D�>�D�~�Dξ�D�  D�AHDπ D�� D���D�@ DЁHD�� D�  D�@ Dр D�� D�  D�AHDҀ D�� D�HD�@ D�~�D�� D�  D�>�D�~�D�� D���D�>�D�~�D�� D�  D�@ Dր D�� D���D�@ DׁHD��HD�  D�>�D�~�D��HD�  D�@ D�~�D�� D�HD�@ Dڀ Dھ�D���D�>�D�}qD۾�D�  D�AHD܁HD��HD�  D�>�D݀ D��HD�HD�AHDށHD�� D��qD�>�D߀ D�� D�  D�>�D�~�DྸD��qD�>�D�~�D�� D�  D�>�D�~�D�� D���D�@ D� D��HD�HD�AHD�~�D侸D�  D�>�D�~�D��HD�HD�AHD�HD�� D���D�>�D� D��HD�  D�@ D�~�D辸D�HD�AHD�HD�� D�  D�AHD� D�� D�HD�@ D�~�D�� D�HD�AHD� D쾸D���D�@ D� D��qD�  D�@ D� D�D�  D�=qD�~�D�� D�  D�AHD��HD��HD�HD�AHD� D�qD���D�@ D� D�D�HD�@ D�~�D�� G�O�?.{?aG�?�z�?�{?Ǯ?��@�@z�@&ff@8Q�@J=q@Y��@k�@}p�@�ff@���@�Q�@�  @���@���@��H@��
@���@�@޸R@���@��@��HAG�AA
=qA�RA33A�A(�A ��A%�A)��A.�RA2�\A7
=A;�A@��ADz�AHQ�AL��AQ�AUAY��A^{Ab�\AfffAj�HAn�RAs33Aw�A|(�A�Q�A��\A���A��RA�G�A�33A��A��A���A�(�A�ffA���A��HA��A�\)A�G�A��
A�A�Q�A��\A�z�A�
=A���A�33A�p�A�\)A��A�(�A��RA�Q�A��HA��A�\)A�G�A��
A�{A�Q�Aҏ\A���AָRA�G�AۅA�p�A߮A�=qA�(�A�RA���A��HA��A�\)A��A�(�A�ffA���A��HA�p�A��B ��B�B33B(�BG�BffB�B��B	B
�HB  BG�BffB\)Bz�B��B�RB  B��B{B
=B(�Bp�B�\B�B��BB�HB   B!�B"ffB#\)B$z�B%��B&�HB((�B)�B*ffB+\)B,��B-�B/
=B0  B1G�B2�\B3�B4��B5�B733B8Q�B9G�B:�\B;�
B=�B>=qB?\)B@z�BA��BB�HBC�
BE�BF=qBG�BH��BIBK
=BL(�BMp�BN�\BO�BQ�BR=qBS\)BT��BUBW
=BX(�BYG�BZ�RB[�
B]�B^ffB_�B`��Ba�Bc\)Bdz�Be��Bf�HBh  Bi�BjffBk�Bl��Bm�Bo\)Bp��BqBr�HBt(�Bup�Bv�RBw�
By�Bz=qB{\)B|��B~{B33B�(�B���B�\)B��B���B�33B��
B�z�B���B��B�Q�B��HB��B�{B��RB�\)B��B��\B�33B��
B�ffB�
=B���B�=qB��HB�p�B�{B��RB�G�B��B�z�B��B�B�ffB���B��B�(�B���B�\)B�  B��\B�G�B�B�z�B�
=B���B�=qB���B�p�B�  B���B�33B��
B�z�B���B��B�(�B��HB�\)B�  B���B�G�B�B�ffB���B��B�=qB��HB�p�B�(�B��RB�\)B��B��\B�33B�B�Q�B�
=B���B�=qB���B�p�B�  B���B�G�B��B��\B��B�B�ffB���B���B�(�B��RB�p�B��B���B�33B��
B�z�B��B��B�Q�B���B�p�B�{B��RB�\)B��B\B�33B��
B�ffB�
=BŮB�=qB��HBǅB�(�Bȣ�B�\)B��Bʣ�B��B�B�ffB�
=B͙�B�=qB��HBυB�{BиRB�\)B�  Bң�B�33B��
B�z�B�
=BծB�Q�B��HBׅB�{BظRB�\)B�  Bڣ�B�G�B��
B�z�B��B�B�z�B���B߮B�Q�B���B�B�=qB���B�B�(�B�RB�\)B�{B��B�\)B��B�\B�33B��
B�ffB��B뙚B�Q�B��HB홚B�(�B���B�\)B�{B��B�33B��
B�z�B��B�B�Q�B���B���B�=qB���B�\)B�{B���B�G�B��B��\B�33B�B�ffB�
=B��B�=qB��HB��C {C \)C ��C ��C=qC��C�C=qC�C�
C(�Cp�C��C{Cp�C�C
=CQ�C��C��C=qC��C�HC33C�CC
=CffC�C	
=C	\)C	��C	��C
Q�C
��C
�C=qC�\C�HC(�Cz�CC{C\)C��C��C33C�C�RC��C33Cp�C��C�
C  C(�C\)C�C�RC�C{C=qCffC��C��C��C(�CQ�Cz�C�C�
C
=C33C\)C�\C�RC�C{C=qCffC�\CC��C�CG�Cp�C��C�
C
=C33CffC�\C�RC��C�C\)C�C�C�HC{C=qCp�C��C�
C  C33CffC�\CC��C�CQ�Cz�C�RC�
C{C33CffC��CC��C�CQ�Cz�C��C�
C  C(�C\)Cz�C�C�HC  C=qC\)C�\CC�C{CG�CffC��C��C   C (�C \)C �C �RC �C!�C!Q�C!�C!�C!�HC"{C"=qC"p�C"��C"�
C#  C#33C#ffC#�\C#�RC#��C$�C$G�C$z�C$��C$�
C%
=C%33C%p�C%��C%��C%��C&(�C&Q�C&�C&C&�C'{C'G�C'p�C'��C'�
C(  C((�C(\)C(�C(�RC(�C){C)=qC)p�C)��C)��C*  C*(�C*\)C*�\C*�RC*�C+�C+G�C+z�C+��C+�
C,
=C,(�C,\)C,�\C,C,�C-�C-G�C-z�C-�C-�HC.
=C.=qC.p�C.��C.��C/  C/33C/\)C/�\C/�RC/�C0�C0G�C0�C0��C0�
C1  C133C1ffC1��C1C1��C2(�C2G�C2z�C2�C2�HC3{C3=qC3ffC3��C3C4  C433C4\)C4�\C4C4��C5(�C5Q�C5�\C5C5�C6(�C6\)C6��C6C7  C7(�C7ffC7��C7��C8
=C8=qC8p�C8��C8�
C9{C9G�C9z�C9�C9�HC:�C:Q�C:�C:C:��C;(�C;\)C;�\C;C<  C<=qC<p�C<��C<�HC={C=G�C=�C=�RC=�C>(�C>ffC>�\C>�
C?
=C?G�C?z�C?�C?�C@(�C@\)C@��C@��CA
=CA=qCAz�CA�RCA�CB�CBQ�CB�CBCC  CC(�CCffCC��CC�
CD
=CD=qCDz�CD�CD�CE�CEQ�CE�\CE�
CF
=CF=qCFz�CF�CF�CG(�CGffCG��CG�HCH{G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                            @�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A��A���A���A���A���A���A���A���A��;A���A���A��A�S�A�33A�-A�&�A�"�A��A�bA�  A��A�ȴA�S�A� �A�%A��RA�O�A�oA���A�n�A�XA�A�A�bA��mA��A��;A��
A�ȴA��RA���A�ffA��A��7A�p�A�"�A��-A���A�ZA��A���A�G�A�bNA�ĜA��^A��A���A�v�A�5?A���A��jA�x�A�O�A��A�t�A��A�v�A�`BA���A�ƨA��A�7LA�M�A�A�bNA�~�A�VA���A�~�A��A�p�A�A���A���A�\)A���A�Q�A���A�n�A��A���A��A���A���A�^5A���A�-A�hsA��mA��A�+A�z�A���A��A�ƨA�ffA��!A�r�A��mA�O�A���A���A���A�S�A��uA���A��A�jA��RA��A�|�A~�+A}�^A|=qA{%Ay��Aw�;AwC�Av5?AtE�Ar�9Aq��ApffAmx�Akp�Ai��Af�Ad  AbȴAbI�Aa�A`ĜA]��A\�AZ��AY�TAYK�AX��AX�AX1'AW��AU��ASAR�!AQ�APA�AOhsAN�!AM�ALI�AK"�AI33AH  AF��AE�;AE�7AE�AD~�AC��AC
=AB=qA@E�A>��A=dZA=�A<�yA<~�A:�A9�A85?A7x�A7O�A7G�A7oA5��A4�\A2��A1�#A1O�A/dZA.  A-��A-"�A,�uA+�7A*1A(��A(�A'
=A&�uA%�
A%VA$VA#S�A"��A ��A bNA �AO�AĜA��A$�A�A�`AQ�A�-A��AbNA �A��A�-A�PA+Az�A1'A��A�A�A�A�A/A��A��A�An�A(�A�;A?}A�A�A
�A
��A	��A	C�A5?AI�A�PAS�A�Ap�A��A�HA1'A   @�^5@��@��@�/@�dZ@���@�(�@��@���@�(�@�w@��@@�+@�^5@��#@�&�@�F@��@柾@���@�9X@�F@�^5@�`B@���@ߍP@�\)@���@��@�p�@�bN@ۍP@ڸR@��T@�p�@؋D@��m@�"�@���@�9X@ӝ�@�+@�
=@��@�ȴ@�V@�hs@�r�@�t�@�{@��@̓u@ˍP@�@ʧ�@�@Ɂ@�p�@�X@�7L@�%@ȴ9@�Q�@���@�o@őh@Ĵ9@��@�C�@§�@��T@��`@��@���@�%@���@���@��j@��
@��P@�|�@�;d@���@�-@���@�`B@�V@��u@� �@�ƨ@�|�@�;d@��@�@��H@�v�@���@�%@���@�1@��w@��P@���@�ȴ@�X@��@���@���@��D@��D@��@�z�@�bN@�j@�r�@�j@�  @��
@���@��P@�;d@�v�@���@��@��u@�1'@�t�@�o@�J@���@��7@���@�/@�j@�K�@�^5@�@�=q@�E�@�=q@�M�@��@��m@���@�@���@�O�@�%@���@��@�(�@�1@��
@��@�=q@�-@�-@�J@��@��@���@�r�@�I�@��@��
@���@��P@�l�@�C�@��y@�V@�p�@��@�1'@�+@�ȴ@���@�ȴ@�1'@�b@��P@��H@�"�@���@���@���@��@�l�@��R@�@�@��h@�hs@�hs@�O�@��@�Ĝ@��@�Z@���@���@��y@��H@���@�n�@�5?@��@��h@�hs@�V@�r�@�(�@���@�K�@�
=@��H@���@�J@��^@��7@�O�@��/@��@�z�@�r�@�z�@�b@���@�l�@�
=@��@��\@�v�@�M�@�-@��T@�hs@�7L@���@��u@�(�@��m@���@��F@���@���@�t�@�S�@�"�@���@���@�M�@�5?@��@�`B@��@��`@���@��9@���@�bN@�1'@�1@��;@�ƨ@���@��@�dZ@�o@���@�~�@�ff@�ff@�=q@�@��@��@���@���@��@�p�@�hs@�O�@��@���@��@�b@l�@+@
=@~��@~5?@}�T@}@}�-@}�@}?}@|�j@|j@|j@{�F@{@z~�@z�@y�^@y��@y&�@x  @w;d@v5?@v@u@tj@s�
@sdZ@s"�@r�H@r�!@rM�@q�#@q�^@q��@q�@pr�@p �@o�;@o��@o��@o�P@oK�@o
=@nȴ@n$�@mp�@m/@l��@lz�@l9X@l�@kƨ@kt�@kS�@j�@j�!@j~�@j^5@j-@i&�@h��@h �@g\)@f�y@fv�@fff@f5?@e@e�@d�@d�@dz�@d�@cƨ@c��@cdZ@co@b^5@a��@a�7@ahs@a&�@`�9@`bN@_�@_�w@_�@_��@_|�@]�@]�h@]�@]p�@]�@\z�@[�m@[�@["�@Z�@Z��@Zn�@ZJ@YG�@X�u@X �@W��@W�@Wl�@W+@V��@V�+@VE�@U�@U��@U@U@U��@U�h@Up�@U`B@U`B@U�@TI�@Sƨ@SdZ@So@So@R�@R��@RM�@Q��@QX@Q7L@P�`@P�@P1'@P �@P1'@P1'@Pb@O�;@O;d@Nȴ@N�+@NE�@M�@M��@M�h@Mp�@M�@L�/@L��@L��@L�j@L�D@L9X@K��@K�F@K�F@Kt�@KC�@Ko@J�H@J^5@I��@Ihs@IX@I7L@I&�@I%@HĜ@H��@HQ�@G�;@G�w@GK�@G
=@F�y@F��@F��@Fv�@Fv�@FV@FV@F$�@E�@E�T@E��@E@E@E/@Cƨ@C33@C@B�@B�@B��@B�!@B^5@BM�@B=q@B-@B�@BJ@BJ@BJ@BJ@A�^@AG�@@Ĝ@@b@?\)@?�@>E�@=��@=�@=V@<j@;�F@;S�@:^5@9�7@9hs@97L@9%@8Ĝ@8A�@7�@7��@7+@6�R@6E�@6@5�T@5��@5�-@5��@5�h@5O�@5�@4�/@4j@4(�@41@3��@3C�@3o@2�@2�!@2��@2�\@2M�@2�@1�@1�7@0��@0��@0��@01'@0  @/��@/�@/�P@/K�@.��@.�y@.�R@.ff@.$�@-�T@-��@-�@,�/@,�@,I�@+�
@+��@+"�@*��@*��@*�\@*J@)��@)&�@(Ĝ@(Ĝ@(��@(r�@(Q�@(1'@(  @'�@'\)@'+@'
=@&�@&��@&�+@&ff@&@%��@%�h@$�@$�@$z�@$I�@$9X@#��@#��@#�@#dZ@#33@#@"��@"��@"n�@!��@!��@!hs@!�@ ��@ 1'@   @��@�w@��@|�@l�@l�@K�@+@�@
=@�y@ȴ@v�@$�@��@?}@�@�@�j@��@�D@Z@9X@(�@(�@(�@�@�@1@��@�m@ƨ@��@��@�@dZ@33@"�@��@�!@n�@��@��@��@x�@7L@��@�`@Ĝ@Ĝ@�u@�@r�@b@��@�w@l�@K�@��@ȴ@�R@��@V@@�-@p�@O�@V@�j@z�@Z@I�@9X@�
@�F@��@dZ@�H@��@��@n�@-@��@��@�@��@x�@X@&�@��@�`@�u@Q�@A�@1'@ �@b@�@��@;d@�@��@�@��@v�@E�@{@@�@@�T@�h@?}@�@V@��A���A���A���A��A��A��A���A��A���A���A���A���A���A���A���A���A���A��A���A���A���A���A���A���A���A���A�  A���A���A�  A���A���A���A�  A��A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�z�A�jA�p�A�ffA�ffA�ffA�bNA�hsA�;dA�9XA�7LA�5?A�5?A�1'A�/A�/A�/A�/A�33A�1'A�+A�+A�+A�1'A�(�A�-A�(�A�-A�$�A�(�A�&�A�"�A�$�A�&�A�$�A�"�A�"�A��A�"�A��A�"�A��A��A��A��A��A��A��A�{A�bA�VA�bA�JA�bA�
=A�%A�%A�  A���A���A���A���A��A��A��yA��A��mA��TA��;A��#A��
A���A���A���A��-A�z�A�hsA�hsA�bNA�Q�A�?}A�1'A�/A�+A�+A�"�A� �A�"�A�bA�VA�oA�1A�
=A�
=A�
=A�A���A���A���A���A���A��FA�n�A�ZA�\)A�VA�XA�XA�S�A�K�A�-A��A��A��A�{A�
=A�
=A���A���A��A���A��RA���A���A��A�|�A�x�A�jA�ffA�bNA�dZA�bNA�ffA�ZA�XA�Q�A�Q�A�M�A�K�A�G�A�C�A�=qA�;dA�=qA�/A�-A�oA�VA�
=A�A�%A��A��A��yA��`A��`A��;A��HA��#A��;A��A��
A��
A���A��/A��#A��;A��HA��HA��`A��HA��HA��#A���A��
A���A���A���A���A���A�ƨA�ƨA�A�ƨA��wA��wA��FA��^A��^A��FA��-A��FA��-A���A���A���A���A���A��hA�r�A�XA�S�A�K�A�?}A�5?A� �A��A���A�ȴA��wA��A��uA��7A��A�|�A�~�A�z�A�|�A�v�A�z�A�t�A�t�A�ffA�K�A�E�A�E�A�?}A�7LA��A��`A�ƨA��FA��A��RA��!A���A���A���A���A��uA���A��\A��hA��hA��7A�r�A�dZA�A�A�&�A�"�A��A�{A���A��`A��
A��^A��FA��A��A���A���A��hA��DA��A��A�~�A�p�A�$�A��/A�ƨA��^A��RA��A�XA�1A��/A���A�ȴA�ƨA�ȴA���A�A��wA��wA��RA��^A��RA��jA��RA��^A��9A��-A��A���A���A���A���A���A���A���A���A���A���A��+A��DA�z�A�jA�^5A�I�A�K�A�E�A�?}A�A�A�+A�
=A�A�A���A���A���A��A��mA��;A���A�ƨA��A��PA��A��A�~�A�~�A�x�A�x�A�jA�ffA�XA�VA�M�A�K�A�=qA�?}A�;dA�7LA��A�bA���A��A��A��FA��+A�S�A�+A�$�A�JA���A��yA��
A�ȴA��RA��A���A��uA��DA�v�A�dZA�5?A�bA���A���A��A� �A��HA��A�;dA��A��FA�dZA�=qA��A��mA��;A���A���A��wA��A���A�v�A�^5A�=qA��A���A�l�A�M�A�;dA�5?A�(�A�-A�(�A�/A�1'A�;dA�G�A�Q�A�n�A�z�A��uA���A���A��A��mA��A���A��A���A�5?A��/A���A���A���A��7A�~�A�r�A�t�A�l�A�jA�bNA�^5A�XA�VA�S�A�G�A�5?A��A�  A��TA���A�A��jA��A���A���A���A���A���A��\A�~�A�~�A�x�A�z�A�v�A�r�A�n�A�ffA�dZA�\)A�XA�XA�I�A�5?A�&�A��A��A���A���A�z�A�l�A�VA�9XA���A��7A�l�A��A���A���A���A�l�A�5?A�-A�-A�(�A�$�A� �A���A���A��9A��-A��A��A��!A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A��9A��A��-A���A�p�A�?}A�(�A�{A��A�ĜA��A�z�A�^5A�G�A�A���A��mA���A��A��A�S�A�
=A��7A�K�A�G�A�G�A�=qA�-A��A��A��A�oA�{A�
=A�1A�A���A���A��`A��`A��TA��;A��HA��/A��
A���A�ȴA���A�`BA�5?A���A�ȴA��A�5?A��A�bA���A��A��HA��;A���A���A���A���A���A��9A��7A�`BA�&�A��uA�XA�1A���A��9A��!A���A���A��A��PA��7A��\A�z�A�z�A�t�A�r�A�`BA�\)A�XA�XA�K�A�A�A�/A�"�A��RA�x�A�M�A�$�A���A��jA��FA�1'A���A��RA���A�z�A�VA�5?A��A�JA���A��`A��RA�bNA�A�A��A�%A���A�\)A�C�A�7LA�33A�"�A���A���A��-A��A��A�7LA�JA��TA��RA��7A�n�A�G�A�"�A��A�ȴA�ffA�1'A�VA��A���A���A�v�A�VA��A��TA��RA�r�A�hsA�^5A�Q�A�=qA�VA�t�A�G�A�=qA�33A�+A��A��A��A�JA���A���A��DA�x�A�r�A�p�A�`BA�Q�A�?}A��A�A��`A���A�ƨA��9A��A���A�z�A�ffA�^5A�Q�A�/A��A�JA���A��yA��A�ȴA��FA���A��\A�t�A�jA�^5A�VA�C�A� �A�bA��A���A���A�r�A�?}A�oA���A���A��7A�n�A�K�A��A��mA��^A�z�A�S�A�?}G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                            A��A��A���A���A���A���A���A���A���A��;A���A���A��A�S�A�33A�-A�&�A�"�A��A�bA�  A��A�ȴA�S�A� �A�%A��RA�O�A�oA���A�n�A�XA�A�A�bA��mA��A��;A��
A�ȴA��RA���A�ffA��A��7A�p�A�"�A��-A���A�ZA��A���A�G�A�bNA�ĜA��^A��A���A�v�A�5?A���A��jA�x�A�O�A��A�t�A��A�v�A�`BA���A�ƨA��A�7LA�M�A�A�bNA�~�A�VA���A�~�A��A�p�A�A���A���A�\)A���A�Q�A���A�n�A��A���A��A���A���A�^5A���A�-A�hsA��mA��A�+A�z�A���A��A�ƨA�ffA��!A�r�A��mA�O�A���A���A���A�S�A��uA���A��A�jA��RA��A�|�A~�+A}�^A|=qA{%Ay��Aw�;AwC�Av5?AtE�Ar�9Aq��ApffAmx�Akp�Ai��Af�Ad  AbȴAbI�Aa�A`ĜA]��A\�AZ��AY�TAYK�AX��AX�AX1'AW��AU��ASAR�!AQ�APA�AOhsAN�!AM�ALI�AK"�AI33AH  AF��AE�;AE�7AE�AD~�AC��AC
=AB=qA@E�A>��A=dZA=�A<�yA<~�A:�A9�A85?A7x�A7O�A7G�A7oA5��A4�\A2��A1�#A1O�A/dZA.  A-��A-"�A,�uA+�7A*1A(��A(�A'
=A&�uA%�
A%VA$VA#S�A"��A ��A bNA �AO�AĜA��A$�A�A�`AQ�A�-A��AbNA �A��A�-A�PA+Az�A1'A��A�A�A�A�A/A��A��A�An�A(�A�;A?}A�A�A
�A
��A	��A	C�A5?AI�A�PAS�A�Ap�A��A�HA1'A   @�^5@��@��@�/@�dZ@���@�(�@��@���@�(�@�w@��@@�+@�^5@��#@�&�@�F@��@柾@���@�9X@�F@�^5@�`B@���@ߍP@�\)@���@��@�p�@�bN@ۍP@ڸR@��T@�p�@؋D@��m@�"�@���@�9X@ӝ�@�+@�
=@��@�ȴ@�V@�hs@�r�@�t�@�{@��@̓u@ˍP@�@ʧ�@�@Ɂ@�p�@�X@�7L@�%@ȴ9@�Q�@���@�o@őh@Ĵ9@��@�C�@§�@��T@��`@��@���@�%@���@���@��j@��
@��P@�|�@�;d@���@�-@���@�`B@�V@��u@� �@�ƨ@�|�@�;d@��@�@��H@�v�@���@�%@���@�1@��w@��P@���@�ȴ@�X@��@���@���@��D@��D@��@�z�@�bN@�j@�r�@�j@�  @��
@���@��P@�;d@�v�@���@��@��u@�1'@�t�@�o@�J@���@��7@���@�/@�j@�K�@�^5@�@�=q@�E�@�=q@�M�@��@��m@���@�@���@�O�@�%@���@��@�(�@�1@��
@��@�=q@�-@�-@�J@��@��@���@�r�@�I�@��@��
@���@��P@�l�@�C�@��y@�V@�p�@��@�1'@�+@�ȴ@���@�ȴ@�1'@�b@��P@��H@�"�@���@���@���@��@�l�@��R@�@�@��h@�hs@�hs@�O�@��@�Ĝ@��@�Z@���@���@��y@��H@���@�n�@�5?@��@��h@�hs@�V@�r�@�(�@���@�K�@�
=@��H@���@�J@��^@��7@�O�@��/@��@�z�@�r�@�z�@�b@���@�l�@�
=@��@��\@�v�@�M�@�-@��T@�hs@�7L@���@��u@�(�@��m@���@��F@���@���@�t�@�S�@�"�@���@���@�M�@�5?@��@�`B@��@��`@���@��9@���@�bN@�1'@�1@��;@�ƨ@���@��@�dZ@�o@���@�~�@�ff@�ff@�=q@�@��@��@���@���@��@�p�@�hs@�O�@��@���@��@�b@l�@+@
=@~��@~5?@}�T@}@}�-@}�@}?}@|�j@|j@|j@{�F@{@z~�@z�@y�^@y��@y&�@x  @w;d@v5?@v@u@tj@s�
@sdZ@s"�@r�H@r�!@rM�@q�#@q�^@q��@q�@pr�@p �@o�;@o��@o��@o�P@oK�@o
=@nȴ@n$�@mp�@m/@l��@lz�@l9X@l�@kƨ@kt�@kS�@j�@j�!@j~�@j^5@j-@i&�@h��@h �@g\)@f�y@fv�@fff@f5?@e@e�@d�@d�@dz�@d�@cƨ@c��@cdZ@co@b^5@a��@a�7@ahs@a&�@`�9@`bN@_�@_�w@_�@_��@_|�@]�@]�h@]�@]p�@]�@\z�@[�m@[�@["�@Z�@Z��@Zn�@ZJ@YG�@X�u@X �@W��@W�@Wl�@W+@V��@V�+@VE�@U�@U��@U@U@U��@U�h@Up�@U`B@U`B@U�@TI�@Sƨ@SdZ@So@So@R�@R��@RM�@Q��@QX@Q7L@P�`@P�@P1'@P �@P1'@P1'@Pb@O�;@O;d@Nȴ@N�+@NE�@M�@M��@M�h@Mp�@M�@L�/@L��@L��@L�j@L�D@L9X@K��@K�F@K�F@Kt�@KC�@Ko@J�H@J^5@I��@Ihs@IX@I7L@I&�@I%@HĜ@H��@HQ�@G�;@G�w@GK�@G
=@F�y@F��@F��@Fv�@Fv�@FV@FV@F$�@E�@E�T@E��@E@E@E/@Cƨ@C33@C@B�@B�@B��@B�!@B^5@BM�@B=q@B-@B�@BJ@BJ@BJ@BJ@A�^@AG�@@Ĝ@@b@?\)@?�@>E�@=��@=�@=V@<j@;�F@;S�@:^5@9�7@9hs@97L@9%@8Ĝ@8A�@7�@7��@7+@6�R@6E�@6@5�T@5��@5�-@5��@5�h@5O�@5�@4�/@4j@4(�@41@3��@3C�@3o@2�@2�!@2��@2�\@2M�@2�@1�@1�7@0��@0��@0��@01'@0  @/��@/�@/�P@/K�@.��@.�y@.�R@.ff@.$�@-�T@-��@-�@,�/@,�@,I�@+�
@+��@+"�@*��@*��@*�\@*J@)��@)&�@(Ĝ@(Ĝ@(��@(r�@(Q�@(1'@(  @'�@'\)@'+@'
=@&�@&��@&�+@&ff@&@%��@%�h@$�@$�@$z�@$I�@$9X@#��@#��@#�@#dZ@#33@#@"��@"��@"n�@!��@!��@!hs@!�@ ��@ 1'@   @��@�w@��@|�@l�@l�@K�@+@�@
=@�y@ȴ@v�@$�@��@?}@�@�@�j@��@�D@Z@9X@(�@(�@(�@�@�@1@��@�m@ƨ@��@��@�@dZ@33@"�@��@�!@n�@��@��@��@x�@7L@��@�`@Ĝ@Ĝ@�u@�@r�@b@��@�w@l�@K�@��@ȴ@�R@��@V@@�-@p�@O�@V@�j@z�@Z@I�@9X@�
@�F@��@dZ@�H@��@��@n�@-@��@��@�@��@x�@X@&�@��@�`@�u@Q�@A�@1'@ �@b@�@��@;d@�@��@�@��@v�@E�@{@@�@@�T@�h@?}@�@VG�O�A���A���A���A��A��A��A���A��A���A���A���A���A���A���A���A���A���A��A���A���A���A���A���A���A���A���A�  A���A���A�  A���A���A���A�  A��A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�z�A�jA�p�A�ffA�ffA�ffA�bNA�hsA�;dA�9XA�7LA�5?A�5?A�1'A�/A�/A�/A�/A�33A�1'A�+A�+A�+A�1'A�(�A�-A�(�A�-A�$�A�(�A�&�A�"�A�$�A�&�A�$�A�"�A�"�A��A�"�A��A�"�A��A��A��A��A��A��A��A�{A�bA�VA�bA�JA�bA�
=A�%A�%A�  A���A���A���A���A��A��A��yA��A��mA��TA��;A��#A��
A���A���A���A��-A�z�A�hsA�hsA�bNA�Q�A�?}A�1'A�/A�+A�+A�"�A� �A�"�A�bA�VA�oA�1A�
=A�
=A�
=A�A���A���A���A���A���A��FA�n�A�ZA�\)A�VA�XA�XA�S�A�K�A�-A��A��A��A�{A�
=A�
=A���A���A��A���A��RA���A���A��A�|�A�x�A�jA�ffA�bNA�dZA�bNA�ffA�ZA�XA�Q�A�Q�A�M�A�K�A�G�A�C�A�=qA�;dA�=qA�/A�-A�oA�VA�
=A�A�%A��A��A��yA��`A��`A��;A��HA��#A��;A��A��
A��
A���A��/A��#A��;A��HA��HA��`A��HA��HA��#A���A��
A���A���A���A���A���A�ƨA�ƨA�A�ƨA��wA��wA��FA��^A��^A��FA��-A��FA��-A���A���A���A���A���A��hA�r�A�XA�S�A�K�A�?}A�5?A� �A��A���A�ȴA��wA��A��uA��7A��A�|�A�~�A�z�A�|�A�v�A�z�A�t�A�t�A�ffA�K�A�E�A�E�A�?}A�7LA��A��`A�ƨA��FA��A��RA��!A���A���A���A���A��uA���A��\A��hA��hA��7A�r�A�dZA�A�A�&�A�"�A��A�{A���A��`A��
A��^A��FA��A��A���A���A��hA��DA��A��A�~�A�p�A�$�A��/A�ƨA��^A��RA��A�XA�1A��/A���A�ȴA�ƨA�ȴA���A�A��wA��wA��RA��^A��RA��jA��RA��^A��9A��-A��A���A���A���A���A���A���A���A���A���A���A��+A��DA�z�A�jA�^5A�I�A�K�A�E�A�?}A�A�A�+A�
=A�A�A���A���A���A��A��mA��;A���A�ƨA��A��PA��A��A�~�A�~�A�x�A�x�A�jA�ffA�XA�VA�M�A�K�A�=qA�?}A�;dA�7LA��A�bA���A��A��A��FA��+A�S�A�+A�$�A�JA���A��yA��
A�ȴA��RA��A���A��uA��DA�v�A�dZA�5?A�bA���A���A��A� �A��HA��A�;dA��A��FA�dZA�=qA��A��mA��;A���A���A��wA��A���A�v�A�^5A�=qA��A���A�l�A�M�A�;dA�5?A�(�A�-A�(�A�/A�1'A�;dA�G�A�Q�A�n�A�z�A��uA���A���A��A��mA��A���A��A���A�5?A��/A���A���A���A��7A�~�A�r�A�t�A�l�A�jA�bNA�^5A�XA�VA�S�A�G�A�5?A��A�  A��TA���A�A��jA��A���A���A���A���A���A��\A�~�A�~�A�x�A�z�A�v�A�r�A�n�A�ffA�dZA�\)A�XA�XA�I�A�5?A�&�A��A��A���A���A�z�A�l�A�VA�9XA���A��7A�l�A��A���A���A���A�l�A�5?A�-A�-A�(�A�$�A� �A���A���A��9A��-A��A��A��!A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A��9A��A��-A���A�p�A�?}A�(�A�{A��A�ĜA��A�z�A�^5A�G�A�A���A��mA���A��A��A�S�A�
=A��7A�K�A�G�A�G�A�=qA�-A��A��A��A�oA�{A�
=A�1A�A���A���A��`A��`A��TA��;A��HA��/A��
A���A�ȴA���A�`BA�5?A���A�ȴA��A�5?A��A�bA���A��A��HA��;A���A���A���A���A���A��9A��7A�`BA�&�A��uA�XA�1A���A��9A��!A���A���A��A��PA��7A��\A�z�A�z�A�t�A�r�A�`BA�\)A�XA�XA�K�A�A�A�/A�"�A��RA�x�A�M�A�$�A���A��jA��FA�1'A���A��RA���A�z�A�VA�5?A��A�JA���A��`A��RA�bNA�A�A��A�%A���A�\)A�C�A�7LA�33A�"�A���A���A��-A��A��A�7LA�JA��TA��RA��7A�n�A�G�A�"�A��A�ȴA�ffA�1'A�VA��A���A���A�v�A�VA��A��TA��RA�r�A�hsA�^5A�Q�A�=qA�VA�t�A�G�A�=qA�33A�+A��A��A��A�JA���A���A��DA�x�A�r�A�p�A�`BA�Q�A�?}A��A�A��`A���A�ƨA��9A��A���A�z�A�ffA�^5A�Q�A�/A��A�JA���A��yA��A�ȴA��FA���A��\A�t�A�jA�^5A�VA�C�A� �A�bA��A���A���A�r�A�?}A�oA���A���A��7A�n�A�K�A��A��mA��^A�z�A�S�A�?}G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                            ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�6B�6B��B�6B�B��B�dB��B��B�0B��B�B�B��B�LB�B��B�$B�0B��B�3B��B��B�B��B�&B�B�BB�QB��B��B�B�`B��B�xB��B��B;BuB�BGB�B�VB�>B�2B��B�MB�|B��B�B�cB��B� B�"B�B�B�KB�B�DB��B��B�fB�B�ZB�&BޞB�#B��BϫB�OB� BĜB�NB�B�B��B�BچBҽBȀB��B��B~�B~(B~�BqABc�B`�B]�BL0BGzB1'B'�BB�PB��B��B��B�UB�nB�7B��B�fB�Bs�Bd&BZ�BPBE�B=�B8�B5�B/B*�B=B	�B�B iB��B�B�B͟B�9B�wB��B�!B�FB��B�qB��B�YB�oBzxBkQB\�BQBA B/B(�B%B!-B�B
rB{B
�>B
�TB
��B
�)B
�B
�B
�B
��B
�&B
ɺB
�tB
��B
��B
�B
��B
��B
��B
�_B
��B
��B
{�B
y�B
wfB
sMB
p;B
l�B
h
B
e`B
Y�B
V9B
P�B
MjB
L�B
G�B
A�B
<jB
8�B
6zB
5�B
49B
2�B
+kB
'�B
�B
1B
�B

=B
�B
�B	��B	�JB	��B	�B	��B	�sB	�B	��B	��B	�QB	�gB	�HB	��B	�#B	ȀB	�zB	�9B	�mB	�'B	�HB	��B	�<B	�jB	�dB	�$B	��B	�B	��B	�nB	�FB	�hB	�9B	��B	�aB	�[B	�!B	��B	�OB	�=B	��B	�OB	�*B	��B	�RB	��B	��B	�bB	�VB	��B	��B	�B	�~B	�qB	��B	��B	��B	�7B	��B	��B	��B	�+B	�@B	��B	�4B	�(B	�B	��B	��B	�xB	�	B	� B	��B	��B	�B	�_B	�+B	��B	��B	��B	�YB	��B	��B	�%B	��B	�fB	��B	��B	��B	��B	�\B	��B	��B	��B	�VB	��B	��B	��B	�{B	�1B	�IB	�4B	�B	��B	�B	��B	��B	�B	��B	�0B	�B	��B	��B	�tB	�tB	�B	�XB	�^B	�jB	��B	�jB	��B	��B	�dB	��B	��B	�jB	��B	�3B	��B	�B	��B	��B	�jB	�B	��B	�WB	ܒB	��B	��B	��B	�|B	��B	�BB	�HB	�B	�&B	�B	�B	�mB	�KB	�/B	�B	�MB	�B	��B	��B	��B	�	B	�PB	�"B	�"B	�B	�]B
GB
�B

=B
fB
JB
�B
�B
B
OB
�B
 �B
#B
'B
'B
($B
+kB
1'B
5�B
8�B
:�B
F�B
J#B
K�B
L�B
NpB
P�B
R�B
UgB
VmB
WsB
[�B
\�B
aB
f�B
jB
jKB
h�B
iDB
hsB
f�B
j�B
m�B
n/B
o�B
poB
o�B
n�B
m�B
m�B
m�B
n/B
n/B
o�B
qvB
qvB
qvB
rB
t�B
tB
t�B
u�B
v+B
v�B
v`B
w�B
w�B
y	B
y	B
{JB
|�B
��B
�oB
�B
��B
�PB
��B
��B
�'B
��B
�nB
��B
��B
��B
�B
��B
��B
�hB
��B
�3B
�B
��B
�jB
�<B
��B
�B
��B
��B
�OB
��B
ÖB
�aB
��B
��B
�B
ǮB
�KB
�KB
��B
̘B
̘B
�6B
� B
��B
��B
ԕB
ӏB
�,B
ҽB
��B
ԕB
ӏB
�gB
�mB
֡B
��B
�B
ٴB
��B
�dB
�B
�B
��B
��B
��B
�B
�B
�B
�B
��B
�B
��B
��B
�>B
�B
��B
�"B
��B
��BAB�B�B�B�BB~B�BVB\B�B4B�B�B�BSB�B�B�B7B�BxBBB�B�B!B�B�B �B!bB!�B!�B"4B#B#nB%�B'�B(�B(�B)_B)�B*0B*eB*�B*�B+B+�B,qB-�B.B0�B1�B2�B3�B4nB49B5tB6zB8RB9�B9�B:�B=B=�B>�B?B?�B@BA BAUBB'BA�BC-BC�BCaBC�BD3BDgBD�BEBEBFBF?BEmBEmBE�BF�BG�BGzBHKBH�BH�BI�BJ�BK)BK^BK�BNpBOBO�BQ�BRTBR�BR�BS&BT,BT�BT�BUgBU�BV9BW
BW?BW
BW�BYKBYBZBZBZ�BZ�B[�B\)B\�B\]B\)B\]B_�B_pB_�B_pB`B`�BaBaBaBaBaBa�Ba�Bc Bc�Bc�Bd�BdZBd�Be`Bd�BffBf2Bf2Bf�Bg8Bf�Bg8Bf�Bg8BgmBg8Bg�BiBiyBi�BjKBjBi�BjBj�Bk�Bk�Bk�Bl�Bl�Bm)Bm)Bl�Bl�Bm]Bl�Bn�Bo Bo Bo5Bo�Bo�BpoBoiBp�Bp�BpoBpBpBp;Bp�BqBqABp�Bp�BqBqABq�BsMBsBtBsMBtBs�Bs�Bs�BtTBt�Bt�Bt�Bu�BuZBu�Bu�Bu�Bv+Bv`Bu�Bu�Bv`Bu�Bv+Bu�Bu�BuZBv`BxBx�Bx�Bx�Bx8By	By	By�By>ByrBy>ByrBy�By�By�ByrBzxBz�B{JB|B{�B{B}VB}�B}�B}�B}�B~]B~�B�B�B�B�B� B� B�iB��B��B��B�oB��B�AB��B�B�uB��B�AB��B��B�GB��B��B��B��B��B�B�B�SB�SB�B��B��B��B��B�_B��B��B�1B��B�1B�1B��B�lB�lB��B��B�	B�rB�rB��B�xB�xB��B�JB�B��B��B��B��B�B��B��B��B�(B�\B�\B�.B��B��B�.B�.B��B� B�hB�hB�hB��B��B�oB��B�@B�FB�FB�B�FB�FB��B�B�B��B��B��B��B�SB�SB�YB�YB�YB��B�+B��B�eB�eB��B��B�B�B�B�7B�kB�7B�kB�7B�kB�	B�qB�xB��B��B�B�~B��B�~B��B�OB��B��B�B�B�B��B��B��B��B�VB��B�!B�!B�VB��B��B��B�'B��B��B�-B�bB��B��B��B��B��B��B�hB�hB��B��B��B�tB�@B��B��B��B�FB�FB��B�LB�B��B��B�$B�XB��B��B�XB�_B��B��B��B�eB�B�eB�6B��B�kB�6B�kB��B��B��B�qB�B�qB��B�wB�wB��B��B��B��B��B�OB��B��B�UB�UB��B��B��B�'B��B��B��B�[B��B��B��B�-B�6B�6B�dB��B�qB��B�*B�<B�6B��B�qB�B��B�6B��B��B�B�<B�dB��B�0B�0B��B�6B�0B�B��B��B�jB��B�0B�0B��B��B�wB�^B�0B��B��B�B�XB�XB�LB��B�*B��B��B��B��B��B��B�B�9B��B��B�B�LB�B��B��B�B��B�B�$B��B��B��B��B��B��B�B��B��B��B��B�FB�RB��B��B��B�RB�B��B��B��B�$B��B��B�RB��B�$B��B�B�0B��B�B��B��B�<B��B�HB�BB��B��B��B�UB� B�BŢB��B��B�EB�XB�RB��B�)B�dB͟B�pB�BB�B�&B��B��B�BߤB�B�B�BB�B��B�2B��B�B�`B�B��B�B�mB�B�B�B� B��B��B�B��B�;B�B�|B�NB��B�B�BޞB��BޞB�jB�pB�HB��B�sB�>B�B�B��B��B�B� B�B�B�B�B�B�B��B�KB�WB�)B�5B� B�B�B��B�B�B�B�B�B��B��B��B�B�+B��B��B�B�`B��B�B�B�B�B��B�DB��B�B��B�JB��B�JB�JB�xB�B��B�"B��B�(B�.B��B�B iBGBB��BuB iB;BB�B{BB�B;B�BuBBB�B�B�B�BGB�B�B�B{BGBBYB�B��B��B�(B�VB�cBB�.B�>B�B�B��B��B�lB��B��B��B��B�8B��B�2B��B�rB��B�`B�TB�ZB�+B��B�ZB�DB��B�;B�B��B��B�GB�B��B�B��B�vB�B�B�B�TB��B�rB�B��B�B�vB�|B�;B�oB��B�/B�5B�B��B�B� B�]B�]B�B�WB��B�`B�TB��B�yB�B�B�`B�+B��B��B�B�"B�QB�B�B��B��B�B��B�"B�B�B�B��B�B�WB�B�"B�B�B�B�KB��B�B��B�B�B�DB��B�B��B�)B��B�yB�yB�fB�/B�>B�sB�
B�B�8B�B�B�
B�>B�B�B�B�mB�B�fB�8B�`B�8B�B�B��B�B�B�B�,B�B�TB�TB�B��B�B��B��B��B�B�B�fB�HBޞB�B��B�B�5B��B�5BܒB��B�WB�KBںB��B�BB՛B�BBҽB��BچB�
B�KBҽB҉BچB�XB�RB�9B��B��B�B��B�qB��B�qB��B�B��B�B�zB�'B��B��BĜB��B�BɆBȴBȀB�XB͟B��B֡B�HB�B�fB�KB�B�vB�B�B$tB�B��B  B��B�B�5B��B�B�]B�KB�WB��B�B��B��B�B�`B�
B�B�2B��B�vBںB��B�B��B�2B��B��B�2B�9B�[B�2B�NB�TB� BуB�}B��B��B�pB�B͟B�BϫB�0B��BǮB��B��B�XB��B�}B��B�qB�jB��B��B��B�YB��B�B�	B��B�B��B�B��BcB�lB�B|�B~�B� B~�B}�B��B�4B~]B~]BcB~]B�B}�B�B}�B}�B}�B~�B�B~(B|PB~(B}VB�B}VB~�B~]B~�B}�B�oB��B��B}"B{JBu�B�lB�YBs�Bu�Bq�Bs�BjKBp�BkQBtBlWBm�BjKBsMB^5B^�B\�B`vBb�BaBa|B_pBaHB_�BbNB`BB`�BaBa|Ba�B_�B_�B`�B_;B^�B_�B^jB]�B]�Bb�BWsB^jBXEBT,BXEBI�BK^BL�BK�BG�BH�BI�BK)BK^BK�BMBR�BM�BL�BM�BUgBHKB<B<6B6zB49B5�B5?B/�B/�B1�B0�B1'B/OB/B/B1�B+6B*�B)_B*eB$B'�B'B4B!�B!-BeB$B_B:B!�B BJB�B	B�BfBBoB �B�VB�B��B�B��B�BuB�oB�B�sB�fB�mB��B�&B�TB��B��B�DB�B�WB�B�;B�B��B��B�aB�<B��B��B�aB��B˒B�dB�OB��BB�zB�jB��B��B��B��B��B�B�B��B��B�LB��B��B��B�-B�nB��B��B��B�B�xB�B�OB��B��B�	B�=B�_B�+B��B��B�bB��B�{B��B�~B��B��B�B�rB��B�fB�fB�1B��B��B��B�YB�;B��B��B��B�AB{B}�B�4B{By>BzBx8BtTBs�Bn�Bn/Bn/Bm�Bj�BkBf�Bc�BaG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                            G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                            G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     none                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     none                                                                                                                                                                                                                                                            202209061814042022090618140420220906181404202209061814042022090618140420220906181404SI  SI  ARFMARFM                                                                                                                                                2021042619342120210426193421IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021050414043020210504140430QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021050414043020210504140430QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2021101413074720211014130747IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2022090618140720220906181407IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOW  OW  V1.0V1.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2022090618140720220906181407IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2022090618140720220906181407IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                